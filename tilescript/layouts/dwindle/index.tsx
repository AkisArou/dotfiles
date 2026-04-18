import type { LayoutContext } from "@tilescript/sdk/layout";

import "./index.css";

interface DwindleBranchProps {
  remaining: number;
  depth: number;
}

function DwindleBranch({ remaining, depth }: DwindleBranchProps) {
  const axisClass =
    depth % 2 === 0 ? "dwindle-branch--column" : "dwindle-branch--row";

  return (
    <group class={`dwindle-branch ${axisClass}`}>
      <slot class="dwindle-pane" take={1} />
      {remaining > 1 ? (
        <DwindleBranch remaining={remaining - 1} depth={depth + 1} />
      ) : null}
    </group>
  );
}

export default function layout(ctx: LayoutContext) {
  return (
    <workspace id="frame">
      <slot id="master" take={1} class="dwindle-pane" />

      {ctx.windows.length > 1 ? (
        <DwindleBranch remaining={ctx.windows.length - 1} depth={0} />
      ) : null}
    </workspace>
  );
}
