import type { ActionDescriptor } from "./actions";

export type DecorationMode = "auto" | "client" | "server" | "none";
export type AttachDirection = "after" | "before";
export type Direction = "left" | "right" | "up" | "down";
/** Modifier names accepted by spider-wm and mapped to wlroots modifier bits. */
export type ModifierName = "super" | "logo" | "mod4" | "shift" | "ctrl" | "control" | "alt" | "mod1";
export type ModifierToken = ModifierName | "mod";
export type LetterKey =
  | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m"
  | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
  | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M"
  | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z";
export type DigitKey = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
export type ArrowKey = "Left" | "Right" | "Up" | "Down";
export type FunctionKey =
  | "F1" | "F2" | "F3" | "F4" | "F5" | "F6" | "F7" | "F8" | "F9" | "F10" | "F11" | "F12"
  | "F13" | "F14" | "F15" | "F16" | "F17" | "F18" | "F19" | "F20" | "F21" | "F22" | "F23" | "F24";
export type NamedKey =
  | LetterKey
  | DigitKey
  | ArrowKey
  | FunctionKey
  | "Return"
  | "space"
  | "comma"
  | "period"
  | "Escape"
  | "Tab"
  | "BackSpace"
  | "Delete"
  | "Home"
  | "End"
  | "Page_Up"
  | "Page_Down"
  | `XF86${string}`;
/**
 * Binding keys use XKB keysym names, parsed through xkbcommon
 * (`xkb_keysym_from_name`).
 */
export type Key = NamedKey | (string & {});
export type BindingKey = ModifierToken | Key;
export type Binding = [BindingKey, ...BindingKey[]];
/**
 * Input match keys are spider-wm config keys: `*`, `type:<kind>`, or an exact
 * device identifier string.
 */
export type InputMatch =
  | "*"
  | `type:${"keyboard" | "pointer" | "touchpad" | "touch"}`
  | string;

export interface OutputConfig {
  /** Output mode string in spider-wm format: WIDTHxHEIGHT[@REFRESHHz]. */
  mode?: string;
  /** wlroots output scale. */
  scale?: number;
  /** wl_output transform name. */
  transform?: "normal" | "90" | "180" | "270" | "flipped" | "flipped-90" | "flipped-180" | "flipped-270";
  /** Output layout position in spider-wm format: XxY or X,Y. */
  position?: string;
  /** wlroots adaptive sync toggle. */
  adaptive_sync?: boolean;
  enabled?: boolean;
}

/** Mapping of output name/description to wlroots-style output config. */
export interface OutputsConfig {
  [name: string]: OutputConfig | undefined;
}

export interface InputConfig {
  /** XKB rule names for keyboards. */
  xkb_layout?: string;
  xkb_model?: string;
  xkb_variant?: string;
  xkb_options?: string;
  /** wlroots keyboard repeat settings. */
  repeat_rate?: number;
  repeat_delay?: number;
  /** libinput pointer/touchpad settings. */
  natural_scroll?: boolean;
  tap?: boolean;
  drag_lock?: boolean;
  accel_profile?: "flat" | "adaptive";
  pointer_accel?: number;
  left_handed?: boolean;
  middle_emulation?: boolean;
  dwt?: boolean;
}

/** Keyboard config uses XKB rule names and wlroots repeat settings. */
export interface KeyboardInputConfig extends InputConfig {
  xkb_layout?: string;
  xkb_model?: string;
  xkb_variant?: string;
  xkb_options?: string;
  repeat_rate?: number;
  repeat_delay?: number;
}

/** Pointer config uses libinput-style pointer settings. */
export interface PointerInputConfig extends InputConfig {
  accel_profile?: "flat" | "adaptive";
  pointer_accel?: number;
  left_handed?: boolean;
  middle_emulation?: boolean;
}

/** Touchpad config uses libinput-style touchpad settings. */
export interface TouchpadInputConfig extends PointerInputConfig {
  natural_scroll?: boolean;
  tap?: boolean;
  drag_lock?: boolean;
  dwt?: boolean;
}

export interface TouchInputConfig extends InputConfig {}

/** Mapping of spider-wm input match keys to input config entries. */
export interface InputsConfig {
  [name: string]: InputConfig | undefined;
  "*"?: InputConfig;
  "type:keyboard"?: KeyboardInputConfig;
  "type:pointer"?: PointerInputConfig;
  "type:touchpad"?: TouchpadInputConfig;
  "type:touch"?: TouchInputConfig;
}

export interface RuleConfig {
  app_id?: string;
  title?: string;
  tags?: number | string | Array<number | string>;
  floating?: boolean;
  fullscreen?: boolean;
  monitor?: number | string;
}

export type RulesConfig = RuleConfig[];

export interface BindingEntry {
  bind: Binding;
  action: ActionDescriptor;
}

export interface BindingsConfig {
  mod?: ModifierName;
  entries?: BindingEntry[];
}

export interface OptionsConfig {
  layouts_dir?: string;
  source_layouts_dir?: string;
  sloppyfocus?: boolean;
  decorations?: DecorationMode;
  snapshot_fadeout_ms?: number;
  attach?: AttachDirection;
}

export interface LayoutsConfig {
  default?: string;
  per_tag?: string[];
  per_monitor?: Record<string, string>;
}

export interface SpiderWMConfig {
  tags?: string[];
  options?: OptionsConfig;
  layouts?: LayoutsConfig;
  outputs?: OutputsConfig;
  inputs?: InputsConfig;
  rules?: RulesConfig;
  bindings?: BindingsConfig;
  autostart?: string[];
  autostart_once?: string[];
}
