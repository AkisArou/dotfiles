// Which profile a launch uses: the role's models from settings.json, chosen the
// way the role says, among the harnesses available now. Availability is host
// state, not configuration: the Claude probe marks it, and the owner can. A
// launch records the profile it resolved, so recovery relaunches exactly that.
import { z } from "zod";
import { capability, Cell, provide } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import type { WorkflowStore } from "signalbox/ext/assignments";
import { harnessLabels, harnesses, profileId } from "./models.ts";
import type { Harness, Profile, Role } from "./models.ts";
import type { RoleSettings, SettingsFile } from "./settings.ts";

const unavailableSchema = z.partialRecord(
  z.enum(harnesses),
  z.object({ reason: z.string(), until: z.number().optional() }),
);
export type Unavailable = z.infer<typeof unavailableSchema>;

/** What a launch uses: the profile itself, frozen at the moment it was resolved. */
export type Resolved =
  | { readonly profile: Profile }
  | { readonly wait: string; readonly retryAt?: number };

export interface Choosing {
  /** The coordinator's choice for this task (a profile id), honoured under pick "coordinator". */
  readonly preferred?: string | undefined;
  /** Live agents of the role per profile id, balanced under pick "spread". */
  readonly inUse?: ReadonlyMap<string, number>;
  readonly now?: number;
}

/**
 * first: the first model whose harness is available, in the role's order.
 * spread: the available model with the fewest live agents of the role (ties in order).
 * coordinator: the coordinator's named model while it is listed and available, else first.
 */
export function chooseProfile(
  role: Role,
  settings: RoleSettings,
  unavailable: Unavailable,
  choosing: Choosing = {},
): Resolved {
  const now = choosing.now ?? Date.now();
  const blocked = (harness: Harness) => {
    const state = unavailable[harness];
    return state && (state.until === undefined || state.until > now) ? state : undefined;
  };
  const open = settings.models.filter((profile) => !blocked(profile.harness));
  if (!open.length) {
    const down = [...new Set(settings.models.map((profile) => profile.harness))];
    const until = down
      .map((harness) => blocked(harness)!.until)
      .filter((value) => value !== undefined);
    return {
      wait: `Every ${role} model is unavailable: ${down.map((harness) => `${harnessLabels[harness]} (${blocked(harness)!.reason})`).join(", ")}`,
      ...(until.length ? { retryAt: Math.min(...until) } : {}),
    };
  }
  if (settings.pick === "coordinator" && choosing.preferred) {
    const named = open.find((profile) => profileId(profile) === choosing.preferred);
    if (named) return { profile: named };
  }
  if (settings.pick === "spread") {
    const use = (profile: Profile) => choosing.inUse?.get(profileId(profile)) ?? 0;
    return { profile: open.reduce((best, profile) => (use(profile) < use(best) ? profile : best)) };
  }
  return { profile: open[0]! };
}

export interface Profiles {
  readonly unavailable: Readable<Unavailable>;
  resolve(role: Role, choosing?: Choosing): Resolved;
  /** Mark a harness unavailable (usage exhausted, login expired), or available again with null. */
  setAvailability(
    harness: Harness,
    state: { reason: string; until?: number } | null,
  ): Promise<Unavailable>;
}

const NAMESPACE = "nts/availability";
const KEY = "unavailable";

export function profiles(options: {
  readonly state: Capability<WorkflowStore>;
  readonly settings: Capability<SettingsFile>;
}): { readonly extension: Extension; readonly service: Capability<Profiles> } {
  const service = capability<Profiles>("nts/profiles");
  const cell = new Cell<Unavailable>({});
  let store: WorkflowStore;
  let settings: SettingsFile;
  const instance: Profiles = {
    unavailable: cell,
    resolve(role, choosing) {
      const reading = cell.get();
      return chooseProfile(
        role,
        settings.current().roles[role],
        reading.status === "ready" ? reading.value : {},
        choosing,
      );
    },
    async setAvailability(harness, state) {
      const next = await store.transaction(NAMESPACE, (tx) => {
        const unavailable = { ...unavailableSchema.parse(tx.get(KEY) ?? {}) };
        if (state) unavailable[harness] = state;
        else delete unavailable[harness];
        tx.set(KEY, unavailable as Json);
        return unavailable;
      });
      cell.set(next);
      return next;
    },
  };
  return {
    service,
    extension: {
      id: "nts-profiles",
      requires: [options.state, options.settings],
      provides: [provide(service, instance)],
      async start(context) {
        store = context.get(options.state);
        settings = context.get(options.settings);
        cell.set(
          await store.transaction(NAMESPACE, (tx) => unavailableSchema.parse(tx.get(KEY) ?? {})),
        );
      },
    },
  };
}
