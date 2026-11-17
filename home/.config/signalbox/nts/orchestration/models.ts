// What each harness can run. A profile is one harness, model and effort; every
// combination in the catalog is offered, so nothing lists profiles by hand.
import { z } from "zod";

export const harnesses = ["claude", "codex"] as const;
export type Harness = (typeof harnesses)[number];
export const roles = ["coordinator", "worker", "reviewer"] as const;
export type Role = (typeof roles)[number];

const claudeEfforts = ["low", "medium", "high", "xhigh", "max"] as const;
const codexEfforts = ["minimal", "low", "medium", "high", "xhigh"] as const;
const claudeModels = {
  "claude-fable-5-2": "Fable 5.2",
  "claude-opus-5-5": "Opus 5.5",
  "claude-sonnet-5-5": "Sonnet 5.5",
  "claude-haiku-5-5": "Haiku 5.5",
} as const;
type ClaudeModel = keyof typeof claudeModels;

export const harnessLabels: Record<Harness, string> = { claude: "Claude Code", codex: "Codex" };

/** One launch's harness, model and effort. Codex without a model uses its own configuration's. */
export const profileSchema = z.discriminatedUnion("harness", [
  z.object({
    harness: z.literal("claude"),
    model: z.enum(Object.keys(claudeModels) as [ClaudeModel, ...ClaudeModel[]]),
    effort: z.enum(claudeEfforts),
  }),
  z.object({
    harness: z.literal("codex"),
    model: z.string().min(1).optional(),
    effort: z.enum(codexEfforts),
  }),
]);
export type Profile = z.infer<typeof profileSchema>;

/** A stable name for a profile: claude/claude-opus-5-5/high, codex/configured/high. */
export function profileId(profile: Profile): string {
  return `${profile.harness}/${profile.model ?? "configured"}/${profile.effort}`;
}

/** A profile's name for lists; `configured` names the model Codex runs when none is given. */
export function profileLabel(profile: Profile, configured?: string): string {
  const model =
    profile.harness === "claude"
      ? claudeModels[profile.model]
      : `Codex ${profile.model ?? configured ?? "(configured model)"}`;
  return `${model} · ${profile.effort}`;
}

/** The profile a name denotes; throws with the accepted form when it is not one. */
export function profileFromId(id: string): Profile {
  const [harness, model, effort, ...rest] = id.split("/");
  const parsed = profileSchema.safeParse({
    harness,
    ...(harness === "codex" && model === "configured" ? {} : { model }),
    effort,
  });
  if (rest.length || !parsed.success)
    throw new Error(
      `${id} is not a profile: use claude/<model>/<effort> or codex/<model|configured>/<effort> (sbn models lists them)`,
    );
  return parsed.data;
}

/** Every combination the catalog offers, Claude Code first. */
export const catalog: readonly Profile[] = [
  ...(Object.keys(claudeModels) as ClaudeModel[]).flatMap((model) =>
    claudeEfforts.map((effort) => ({ harness: "claude" as const, model, effort })),
  ),
  ...codexEfforts.map((effort) => ({ harness: "codex" as const, effort })),
];

/** The launch options a profile passes to its harness adapter. */
export function launchOptions(profile: Profile): { model?: string; effort: string } {
  return { ...(profile.model ? { model: profile.model } : {}), effort: profile.effort };
}
