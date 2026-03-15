export interface WindowInfo {
  id: number;
  appId: string;
  title: string;
  floating: boolean;
  fullscreen: boolean;
  mapped: boolean;
  urgent: boolean;
  tag?: number;
}

export interface MonitorInfo {
  name: string;
  x: number;
  y: number;
  width: number;
  height: number;
}

export interface MonitorSnapshot extends MonitorInfo {
  workspace: WorkspaceInfo | null;
  windows: WindowInfo[];
  focusedWindow: WindowInfo | null;
  focused: boolean;
}

export interface WorkspaceInfo {
  activeTags: number[];
  layout: string;
}

export interface FocusChangeEvent {
  previous: WindowInfo | null;
  current: WindowInfo | null;
  monitor: MonitorInfo | null;
  workspace: WorkspaceInfo | null;
}

export interface WindowEvent {
  window: WindowInfo | null;
  monitor: MonitorInfo | null;
  workspace: WorkspaceInfo | null;
}

export interface WindowTagChangeEvent extends WindowEvent {
  previousTags: number[];
  currentTags: number[];
}

export interface WindowBooleanChangeEvent extends WindowEvent {
  previous: boolean;
  current: boolean;
}

export interface WorkspaceEvent {
  monitor: MonitorInfo | null;
  workspace: WorkspaceInfo | null;
}

export interface ConfigReloadedEvent {}

export interface SpiderWMEventMap {
  "focus-change": FocusChangeEvent;
  "window-created": WindowEvent;
  "window-destroyed": WindowEvent;
  "window-tag-change": WindowTagChangeEvent;
  "window-floating-change": WindowBooleanChangeEvent;
  "window-fullscreen-change": WindowBooleanChangeEvent;
  "tag-change": WorkspaceEvent;
  "layout-change": WorkspaceEvent;
  "config-reloaded": ConfigReloadedEvent;
}

export type SpiderWMEventName = keyof SpiderWMEventMap;
export type SpiderWMEventHandler<K extends SpiderWMEventName> = (
  payload: SpiderWMEventMap[K]
) => void;

export interface SpiderWMEvents {
  on<K extends SpiderWMEventName>(
    event: K,
    handler: SpiderWMEventHandler<K>
  ): void;
  once<K extends SpiderWMEventName>(
    event: K,
    handler: SpiderWMEventHandler<K>
  ): void;
  off<K extends SpiderWMEventName>(
    event: K,
    handler?: SpiderWMEventHandler<K>
  ): boolean;
}

export type Direction = "left" | "right" | "up" | "down";

export interface SpiderWMApi {
  spawn(command: string): boolean;
  reloadConfig(): boolean;
  setLayout(layout: string): boolean;
  cycleLayout(direction?: number): boolean;
  viewTag(tag: number): boolean;
  toggleViewTag(tag: number): boolean;
  toggleFloating(): boolean;
  toggleFullscreen(): boolean;
  focusDirection(direction: Direction): boolean;
  closeWindow(): boolean;
}

export interface SpiderWMState {
  focusedWindow: WindowInfo | null;
  currentMonitor: MonitorSnapshot | null;
  currentWorkspace: WorkspaceInfo | null;
  monitors: MonitorSnapshot[];
  tagNames: string[];
}

export interface SpiderWMQueryApi {
  getState(): SpiderWMState;
  getFocusedWindow(): WindowInfo | null;
  getCurrentMonitor(): MonitorSnapshot | null;
  getCurrentWorkspace(): WorkspaceInfo | null;
}

export const events: SpiderWMEvents;
export const wm: SpiderWMApi;
export const query: SpiderWMQueryApi;
