export interface ActionDescriptor<T = unknown> {
  _action: string;
  _arg: T;
}

export type Direction = "left" | "right" | "up" | "down";
export type NoArgAction = () => ActionDescriptor<undefined>;
export type StringAction = (arg: string) => ActionDescriptor<string>;
export type NumberAction = (arg: number) => ActionDescriptor<number>;
export type DirectionAction = (arg: Direction) => ActionDescriptor<Direction>;

export const spawn: StringAction;
export const quit: NoArgAction;
export const reload_config: NoArgAction;
export const focus_next: NoArgAction;
export const focus_prev: NoArgAction;
export const focus_dir: DirectionAction;
export const swap_dir: DirectionAction;
export const resize_dir: DirectionAction;
export const resize_tiled: DirectionAction;
export const focus_mon_left: NoArgAction;
export const focus_mon_right: NoArgAction;
export const send_mon_left: NoArgAction;
export const send_mon_right: NoArgAction;
export const view_tag: NumberAction;
export const toggle_view_tag: NumberAction;
export const tag: NumberAction;
export const toggle_tag: NumberAction;
export const toggle_floating: NoArgAction;
export const toggle_fullscreen: NoArgAction;
export const set_layout: StringAction;
export const cycle_layout: NoArgAction;
export const move: DirectionAction;
export const resize: DirectionAction;
export const kill_client: NoArgAction;
