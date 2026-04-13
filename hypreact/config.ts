import "@hypreact/sdk/css.d.ts";
import type { HypreactConfig } from "@hypreact/sdk/config";

export default {
  defaultLayout: "master-stack",
  layoutRules: [{ index: 0, layout: "master-stack" }],
  resize: {
    stepPx: 96,
    minBranchSizePx: 120,
  },
} satisfies HypreactConfig;
