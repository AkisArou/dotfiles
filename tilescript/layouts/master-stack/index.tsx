import type { LayoutContext } from "@tilescript/sdk/layout";

import "./index.css";

export default function layout(ctx: LayoutContext) {
  return (
    <workspace>
      <slot take={1} class="master-slot" />

      {ctx.windows.length > 1 ? (
        <group class="stack-group">
          <slot class="stack-slot" />
        </group>
      ) : null}
    </workspace>
  );
}
