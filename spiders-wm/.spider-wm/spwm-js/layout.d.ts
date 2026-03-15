export interface LayoutBaseProps {
  id?: string;
  class?: string;
}

export type LayoutRenderable = LayoutNode | LayoutRenderable[] | null;
export type LayoutChildren = LayoutRenderable | LayoutRenderable[];

export interface LayoutComponentProps {
  children?: LayoutChildren;
}

export interface WorkspaceProps extends LayoutBaseProps, LayoutComponentProps {}

export interface GroupProps extends LayoutBaseProps, LayoutComponentProps {}

export interface SlotProps extends LayoutBaseProps, LayoutComponentProps {
  take?: number;
}

export interface WindowProps extends LayoutBaseProps, LayoutComponentProps {
  match?: string;
}

export interface WorkspaceNode {
  type: "workspace";
  props?: WorkspaceProps;
  children?: LayoutChild[];
}

export interface GroupNode {
  type: "group";
  props?: GroupProps;
  children?: LayoutChild[];
}

export interface SlotNode {
  type: "slot";
  props?: SlotProps;
  children?: LayoutChild[];
}

export interface WindowNode {
  type: "window";
  props?: WindowProps;
  children?: LayoutChild[];
}

export type LayoutNode = WorkspaceNode | GroupNode | SlotNode | WindowNode;
export type LayoutChild = LayoutNode | null;

export interface LayoutWindow {
  id: string;
  app_id?: string | null;
  title?: string | null;
  class?: string | null;
  instance?: string | null;
  role?: string | null;
  shell?: string | null;
  window_type?: string | null;
  floating?: boolean;
  fullscreen?: boolean;
  focused?: boolean;
}

export interface LayoutContext {
  monitor: {
    name: string;
    width: number;
    height: number;
    scale?: number;
  };
  workspace: {
    name: string;
    tags?: string[];
    windowCount: number;
  };
  windows: LayoutWindow[];
  state?: Record<string, unknown>;
}

export type LayoutFn = (ctx: LayoutContext) => LayoutRenderable;
