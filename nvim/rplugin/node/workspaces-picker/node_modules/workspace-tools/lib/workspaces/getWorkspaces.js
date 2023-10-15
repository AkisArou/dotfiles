"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspacesAsync = exports.getWorkspaces = void 0;
const implementations_1 = require("./implementations");
/**
 * Get an array with names, paths, and package.json contents for each package in a workspace.
 * The list of included packages is based on the workspace manager's config file.
 *
 * The method name is somewhat misleading due to the double meaning of "workspace", but it's retained
 * for compatibility. "Workspace" here refers to an individual package, in the sense of the `workspaces`
 * package.json config used by npm/yarn (instead of referring to the entire monorepo).
 */
function getWorkspaces(cwd) {
    const utils = (0, implementations_1.getWorkspaceUtilities)(cwd);
    return (utils === null || utils === void 0 ? void 0 : utils.getWorkspaces(cwd)) || [];
}
exports.getWorkspaces = getWorkspaces;
/**
 * Get an array with names, paths, and package.json contents for each package in a workspace.
 * The list of included packages is based on the workspace manager's config file.
 *
 * The method name is somewhat misleading due to the double meaning of "workspace", but it's retained
 * for compatibility. "Workspace" here refers to an individual package, in the sense of the `workspaces`
 * package.json config used by npm/yarn (instead of referring to the entire monorepo).
 */
async function getWorkspacesAsync(cwd) {
    var _a;
    const utils = (0, implementations_1.getWorkspaceUtilities)(cwd);
    if (!utils) {
        return [];
    }
    if (!utils.getWorkspacesAsync) {
        const managerName = (_a = (0, implementations_1.getWorkspaceManagerAndRoot)(cwd)) === null || _a === void 0 ? void 0 : _a.manager;
        throw new Error(`${cwd} is using ${managerName} which has not been converted to async yet`);
    }
    return utils.getWorkspacesAsync(cwd);
}
exports.getWorkspacesAsync = getWorkspacesAsync;
//# sourceMappingURL=getWorkspaces.js.map