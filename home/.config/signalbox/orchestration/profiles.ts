// Agent profiles and role selections are live data in the host's state, not
// code: changing a role's harness, model or effort applies to the next launch
// without a restart. A launch records the profile it resolved, so recovery
// relaunches exactly that.
import { z } from "zod";
import { capability, Cell, provide } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import type { WorkflowStore } from "signalbox/ext/assignments/index";

export const harnesses = ["claude", "codex"] as const;
export const roles = ["coordinator", "worker", "reviewer"] as const;
export type Harness = (typeof harnesses)[number];
export type Role = (typeof roles)[number];

export const profileSchema = z.object({
  id: z.string().regex(/^[a-z0-9][a-z0-9-]*$/),
  label: z.string().min(1),
  harness: z.enum(harnesses),
  model: z.string().min(1).optional(),
  effort: z.string().min(1).optional(),
});
export type Profile = z.infer<typeof profileSchema>;
const roleSchema = z.object({ profile: z.string(), fallback: z.string().optional() });
const unavailableSchema = z.object({ reason: z.string(), until: z.number().optional() });
export const settingsSchema = z.object({
  revision: z.number().int().nonnegative(),
  profiles: z.array(profileSchema),
  roles: z.object({ coordinator: roleSchema, worker: roleSchema, reviewer: roleSchema }),
  unavailable: z.partialRecord(z.enum(harnesses), unavailableSchema),
});
export type ProfileSettings = z.infer<typeof settingsSchema>;

/** What a launch uses: the profile itself, frozen at the moment it was resolved. */
export type Resolved =
  | { readonly profile: Profile }
  | { readonly wait: string; readonly retryAt?: number };

export interface Profiles {
  readonly settings: Readable<ProfileSettings>;
  resolve(role: Role, now?: number): Promise<Resolved>;
  setProfile(profile: Profile, expectedRevision: number): Promise<ProfileSettings>;
  setRole(
    role: Role,
    selection: { profile: string; fallback?: string },
    expectedRevision: number,
  ): Promise<ProfileSettings>;
  /** Mark a harness unavailable (usage exhausted, login expired), or available again with null. */
  setAvailability(
    harness: Harness,
    state: { reason: string; until?: number } | null,
  ): Promise<ProfileSettings>;
}

const NAMESPACE = "nts/profiles";
const KEY = "settings";

function validate(settings: ProfileSettings): ProfileSettings {
  const parsed = settingsSchema.parse(settings);
  const ids = new Set(parsed.profiles.map((profile) => profile.id));
  if (ids.size !== parsed.profiles.length) throw new Error("Profile IDs must be unique");
  for (const role of roles) {
    const selection = parsed.roles[role];
    for (const id of [selection.profile, ...(selection.fallback ? [selection.fallback] : [])])
      if (!ids.has(id)) throw new Error(`Role ${role} names an unknown profile: ${id}`);
  }
  return parsed;
}

export function profiles(options: {
  readonly state: Capability<WorkflowStore>;
  /** The settings a fresh state starts from; later edits live in the state. */
  readonly initial: Omit<ProfileSettings, "revision" | "unavailable">;
}): { readonly extension: Extension; readonly service: Capability<Profiles> } {
  const service = capability<Profiles>("nts/profiles");
  const cell = new Cell<ProfileSettings>();
  let store: WorkflowStore;
  const write = (update: (current: ProfileSettings) => ProfileSettings, expected?: number) =>
    store.transaction(NAMESPACE, (tx) => {
      const current = settingsSchema.parse(tx.get(KEY));
      if (expected !== undefined && current.revision !== expected)
        throw new Error(`Profile settings changed (revision ${current.revision}); reload and retry`);
      const next = validate({ ...update(current), revision: current.revision + 1 });
      tx.set(KEY, next as unknown as Json);
      return next;
    });
  const instance: Profiles = {
    settings: cell,
    async resolve(role, now = Date.now()) {
      const settings = await store.transaction(NAMESPACE, (tx) => settingsSchema.parse(tx.get(KEY)));
      const blocked = (profile: Profile) => {
        const state = settings.unavailable[profile.harness];
        return state && (state.until === undefined || state.until > now) ? state : undefined;
      };
      const selection = settings.roles[role];
      for (const id of [selection.profile, ...(selection.fallback ? [selection.fallback] : [])]) {
        const profile = settings.profiles.find((candidate) => candidate.id === id)!;
        if (!blocked(profile)) return { profile };
      }
      const primary = settings.profiles.find((profile) => profile.id === selection.profile)!;
      const state = blocked(primary)!;
      return {
        wait: `${role} profile ${primary.id} (${primary.harness}) is unavailable: ${state.reason}`,
        ...(state.until === undefined ? {} : { retryAt: state.until }),
      };
    },
    async setProfile(profile, expected) {
      const next = await write(
        (current) => ({
          ...current,
          profiles: [...current.profiles.filter((old) => old.id !== profile.id), profileSchema.parse(profile)],
        }),
        expected,
      );
      cell.set(next);
      return next;
    },
    async setRole(role, selection, expected) {
      const next = await write(
        (current) => ({ ...current, roles: { ...current.roles, [role]: roleSchema.parse(selection) } }),
        expected,
      );
      cell.set(next);
      return next;
    },
    async setAvailability(harness, state) {
      const next = await write((current) => {
        const unavailable = { ...current.unavailable };
        if (state) unavailable[harness] = unavailableSchema.parse(state);
        else delete unavailable[harness];
        return { ...current, unavailable };
      });
      cell.set(next);
      return next;
    },
  };
  return {
    service,
    extension: {
      id: "nts-profiles",
      requires: [options.state],
      provides: [provide(service, instance)],
      async start(context) {
        store = context.get(options.state);
        const settings = await store.transaction(NAMESPACE, (tx) => {
          const existing = tx.get(KEY);
          if (existing !== undefined) return settingsSchema.parse(existing);
          const fresh = validate({ ...options.initial, revision: 0, unavailable: {} });
          tx.set(KEY, fresh as unknown as Json);
          return fresh;
        });
        cell.set(settings);
      },
    },
  };
}

/** The launch options a profile passes to its harness adapter. */
export function launchOptions(profile: Profile): Json {
  return {
    ...(profile.model ? { model: profile.model } : {}),
    ...(profile.effort ? { effort: profile.effort } : {}),
  };
}
