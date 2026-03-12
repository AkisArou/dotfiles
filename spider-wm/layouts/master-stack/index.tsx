import { LayoutContext } from "spider-wm/layout";
import "./index.css"

export default function layout(ctx: LayoutContext) {
  return (
    <workspace id="root">
      <group id="frame">
        <slot id="master" take={1} />
        {ctx.windows.length > 1 && (
          <group id="stack">
            <slot class="stack-item" />
          </group>
        )}
      </group>
    </workspace>
  );
}
