// The project's agent settings come from one file, settings.json beside this
// application, and from nowhere else. The host reads it at start, follows hand
// edits as they are saved, and writes the changes the TUI, web view and sbn ask
// for. A hand edit that fails the schema is refused with its reason and the last
// good settings stay in force; a change from a client is refused while the file
// has an error or changed since that client read it.
import { createHash } from "node:crypto";
import { watch } from "node:fs";
import { readFile, realpath, rename, writeFile } from "node:fs/promises";
import { basename, dirname, join } from "node:path";
import { z } from "zod";
import { capability, Cell, provide } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import { Conflict } from "signalbox/core/utils";
import { profileId, profileSchema, roles } from "./models.ts";
import { uncoveredAreas } from "./shares.ts";

/** How a launch chooses among a role's models (see chooseProfile in profiles.ts). */
export const picks = ["first", "spread", "coordinator"] as const;
export type Pick = (typeof picks)[number];

const roleSettings = z
  .object({ pick: z.enum(picks), models: z.array(profileSchema).min(1) })
  .strict();
export type RoleSettings = z.infer<typeof roleSettings>;

export const settingsSchema = z
  .object({
    /** Tasks holding a worker at once. 0 starts no build; tasks past their build still finish. */
    workerSlots: z.number().int().min(0).max(16),
    /** Returns to build (review findings, landing refusals) before a task escalates. */
    maxRounds: z.number().int().min(1).max(20),
    roles: z
      .object({ coordinator: roleSettings, worker: roleSettings, reviewer: roleSettings })
      .strict(),
    /** How workerSlots is shared between groups of streams or areas; absent, nothing is split. */
    shares: z
      .array(
        z
          .object({
            name: z.string().min(1),
            share: z.number().min(0),
            areas: z.array(z.string().min(1)),
            /** Streams by their parent issue (#468 is 468): their tasks join this group first. */
            streams: z.array(z.number().int().positive()).optional(),
          })
          .strict(),
      )
      .min(1)
      .optional(),
  })
  .strict()
  .superRefine((settings, issues) => {
    for (const role of roles) {
      const ids = settings.roles[role].models.map(profileId);
      if (new Set(ids).size !== ids.length)
        issues.addIssue({
          code: "custom",
          path: ["roles", role, "models"],
          message: "lists a model twice",
        });
    }
    const shares = settings.shares ?? [];
    if (new Set(shares.map((group) => group.name)).size !== shares.length)
      issues.addIssue({ code: "custom", path: ["shares"], message: "names a group twice" });
    const areas = shares.flatMap((group) => group.areas);
    for (const area of new Set(areas.filter((area, index) => areas.indexOf(area) !== index)))
      issues.addIssue({
        code: "custom",
        path: ["shares"],
        message: `area ${area} is in two groups`,
      });
    for (const [index, group] of shares.entries())
      if (!group.areas.length && !group.streams?.length)
        issues.addIssue({
          code: "custom",
          path: ["shares", index],
          message: `${group.name} names no stream and no area`,
        });
    const streams = shares.flatMap((group) => group.streams ?? []);
    for (const stream of new Set(streams.filter((stream, at) => streams.indexOf(stream) !== at)))
      issues.addIssue({
        code: "custom",
        path: ["shares"],
        message: `stream #${stream} is in two groups`,
      });
    if (shares.length && shares.every((group) => group.share === 0))
      issues.addIssue({ code: "custom", path: ["shares"], message: "every share is 0" });
    if (settings.roles.coordinator.pick !== "first")
      issues.addIssue({
        code: "custom",
        path: ["roles", "coordinator", "pick"],
        message: "must be first: one coordinator runs at a time",
      });
  });
export type Settings = z.infer<typeof settingsSchema>;

export interface SettingsReading {
  readonly path: string;
  /** The content hash of the settings in force; a client's change names the version it read. */
  readonly version: string;
  readonly settings: Settings;
  /** Why the file on disk is not in force (the last good settings are), or null. */
  readonly error: string | null;
}

export interface SettingsFile {
  readonly reading: Readable<SettingsReading>;
  /** The settings in force. */
  current(): Settings;
  /** Writes a change to the file; with version, only if the file is still that version. */
  update(change: (settings: Settings) => unknown, version?: string): Promise<SettingsReading>;
}

const hash = (text: string) => createHash("sha256").update(text).digest("hex").slice(0, 12);
const render = (settings: Settings) => `${JSON.stringify(settings, null, 2)}\n`;
const message = (error: unknown) => (error instanceof Error ? error.message : String(error));

/** The file's settings, or the reason they are refused; shares must cover every known area. */
export function parseSettings(text: string, areas: readonly string[] = []): Settings {
  let json: unknown;
  try {
    json = JSON.parse(text);
  } catch (error) {
    throw new Error(`not JSON: ${message(error)}`);
  }
  const parsed = settingsSchema.safeParse(json);
  if (!parsed.success) throw new Error(z.prettifyError(parsed.error));
  const missing = parsed.data.shares ? uncoveredAreas(parsed.data.shares, areas) : [];
  if (missing.length)
    throw new Error(
      `shares: no group has the area${missing.length > 1 ? "s" : ""} ${missing.join(", ")}`,
    );
  return parsed.data;
}

export function settingsFile(options: {
  readonly path: string;
  /** The repository's area labels, which shares must cover. */
  readonly areas?: readonly string[];
}): {
  readonly extension: Extension;
  readonly service: Capability<SettingsFile>;
  /** The settings in force, for callers outside an extension (capacity, round limits). */
  readonly current: () => Settings;
} {
  const service = capability<SettingsFile>("nts/settings");
  const cell = new Cell<SettingsReading>();
  let path = options.path;
  let last: SettingsReading | undefined;
  let queue: Promise<unknown> = Promise.resolve();
  const serial = <T>(work: () => Promise<T>): Promise<T> => {
    const next = queue.then(work, work);
    queue = next.catch(() => undefined);
    return next;
  };
  const publish = (reading: SettingsReading) => {
    last = reading;
    cell.set(reading);
  };
  /** Reads the file; the first read must succeed, later failures keep the last good settings. */
  const load = () =>
    serial(async () => {
      let text: string;
      try {
        text = await readFile(path, "utf8");
      } catch (error) {
        if (!last) throw new Error(`${path}: ${message(error)}`);
        return publish({ ...last, error: `cannot read the file: ${message(error)}` });
      }
      const version = hash(text);
      if (last && version === last.version && !last.error) return;
      try {
        publish({ path, version, settings: parseSettings(text, options.areas), error: null });
      } catch (error) {
        if (!last) throw new Error(`${path}: ${message(error)}`);
        publish({ ...last, error: message(error) });
      }
    });
  const instance: SettingsFile = {
    reading: cell,
    current: () => {
      if (!last) throw new Error("The settings file has not been read yet");
      return last.settings;
    },
    update: (change, version) =>
      serial(async () => {
        const text = await readFile(path, "utf8");
        let current: Settings;
        try {
          current = parseSettings(text, options.areas);
        } catch (error) {
          throw new Error(`${path} has an error; fix it first: ${message(error)}`);
        }
        if (version !== undefined && version !== hash(text))
          throw new Conflict(`${basename(path)} changed since it was read; reload and retry`);
        const next = parseSettings(JSON.stringify(change(current)), options.areas);
        const content = render(next);
        // Replace the file whole, so the watcher and editors never see half of it.
        const temporary = join(dirname(path), `.${basename(path)}.${process.pid}.tmp`);
        await writeFile(temporary, content);
        await rename(temporary, path);
        const reading = { path, version: hash(content), settings: next, error: null };
        publish(reading);
        return reading;
      }),
  };
  return {
    service,
    current: instance.current,
    extension: {
      id: "nts-settings",
      provides: [provide(service, instance)],
      async start(context) {
        // Watch the real directory: the file is replaced, not written in place.
        path = join(await realpath(dirname(options.path)), basename(options.path));
        await load();
        let timer: NodeJS.Timeout | undefined;
        const watcher = watch(dirname(path), (_event, name) => {
          if (name !== basename(path)) return;
          clearTimeout(timer);
          timer = setTimeout(
            () => void load().catch((error) => context.log.error("settings file", error)),
            100,
          );
        });
        watcher.on("error", (error) => context.log.error("settings watch", error));
        context.onStop(() => {
          clearTimeout(timer);
          watcher.close();
        });
      },
    },
  };
}
