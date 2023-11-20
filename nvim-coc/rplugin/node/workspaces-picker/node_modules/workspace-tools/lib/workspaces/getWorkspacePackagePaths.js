"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspacePackagePathsAsync = exports.getWorkspacePackagePaths = void 0;
const implementations_1 = require("./implementations");
/**
 * Get a list of package folder paths in the workspace. The list of included packages is based on
 * the manager's config file and matching package folders (which must contain package.json) on disk.
 */
function getWorkspacePackagePaths(cwd) {
    const utils = (0, implementations_1.getWorkspaceUtilities)(cwd);
    return (utils === null || utils === void 0 ? void 0 : utils.getWorkspacePackagePaths(cwd)) || [];
}
exports.getWorkspacePackagePaths = getWorkspacePackagePaths;
/**
 * Get a list of package folder paths in the workspace. The list of included packages is based on
 * the manager's config file and matching package folders (which must contain package.json) on disk.
 */
async function getWorkspacePackagePathsAsync(cwd) {
    var _a;
    const utils = (0, implementations_1.getWorkspaceUtilities)(cwd);
    if (!utils) {
        return [];
    }
    if (!utils.getWorkspacePackagePathsAsync) {
        const managerName = (_a = (0, implementations_1.getWorkspaceManagerAndRoot)(cwd)) === null || _a === void 0 ? void 0 : _a.manager;
        throw new Error(`${cwd} is using ${managerName} which has not been converted to async yet`);
    }
    return utils.getWorkspacePackagePathsAsync(cwd);
}
exports.getWorkspacePackagePathsAsync = getWorkspacePackagePathsAsync;
//# sourceMappingURL=getWorkspacePackagePaths.js.map