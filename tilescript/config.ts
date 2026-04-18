import "@tilescript/sdk/css.d.ts";
import type { TilescriptConfig } from "@tilescript/sdk/config";

export default {
  defaultLayout: "master-stack",
  resize: {
    stepPx: 96,
    minBranchSizePx: 120,
  },
} satisfies TilescriptConfig;
