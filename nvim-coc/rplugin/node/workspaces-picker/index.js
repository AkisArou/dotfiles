//@ts-check
const { writeFileSync, readFileSync } = require("fs");
const path = require("path");
const tools = require("workspace-tools");

//:UpdateRemotePlugins

/**
 * @param {InstanceType<typeof import("neovim").NvimPlugin>} plugin
 */
module.exports = (plugin) => {
  plugin.setOptions({ dev: false });

  /**
   * @type {string | undefined}
   */
  let root;

  /**
   * @type {ReturnType<typeof import("workspace-tools").getPnpmWorkspaces> | undefined}
   */
  let currentWorkspaces;

  plugin.registerCommand(
    "GetPnpmWorkspaces",
    async () => {
      try {
        root = root || tools.getWorkspaceRoot(process.cwd());

        if (!root) {
          console.error("No root found");
          return;
        }

        currentWorkspaces = tools.getPnpmWorkspaces(root);
        const workspaceNames = currentWorkspaces.map((ws) => ws.name);

        const args = `{ ${workspaceNames.map((n) => `"${n}"`).join(",")} }`;

        await plugin.nvim.executeLua(`PickTargetWorkspace(${args})`);
      } catch (err) {
        console.error(err);
      }
    },
    { sync: false }
  );

  plugin.registerCommand(
    "GotSelections",
    async (/** @type {string} */ targetAndDep) => {
      try {
        const [target, dep] = targetAndDep.toString().split(",");

        if (!currentWorkspaces) {
          console.error("No currentWorkspaces");
          return;
        }

        const targetWorkspace = currentWorkspaces.find(
          (w) => w.name === target
        );

        const depWorkspace = currentWorkspaces.find((w) => w.name === dep);

        if (!targetWorkspace || !depWorkspace) {
          console.error("Could not find target or dep workspace");
          return;
        }

        const targetPackageJsonPath = path.join(
          targetWorkspace.path,
          "package.json"
        );

        const targetPackageJson = JSON.parse(
          readFileSync(targetPackageJsonPath, "utf8")
        );

        let newTargetDependencies = targetPackageJson.dependencies || {};

        newTargetDependencies[depWorkspace.name] = "workspace:*";

        writeFileSync(
          targetPackageJsonPath,
          JSON.stringify(targetPackageJson, null, 2)
        );

        await plugin.nvim.outWrite("Execute pnpm i to sync the packages");

        currentWorkspaces = undefined;
      } catch (err) {
        console.error(err);
      }
    },
    { sync: false, nargs: "1" }
  );
};
