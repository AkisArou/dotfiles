// nts's verbs for sbn are the kit's, against this project's host.
import { readFileSync } from "node:fs";
import { projectCli } from "@signalbox/kit/cli/verbs";

const project = JSON.parse(readFileSync(new URL("../project.json", import.meta.url), "utf8")) as {
  apiPort: number;
};
process.exitCode = await projectCli(process.argv.slice(2), {
  api: process.env.SIGNALBOX_API_URL ?? `ws://127.0.0.1:${project.apiPort}/rpc`,
});
