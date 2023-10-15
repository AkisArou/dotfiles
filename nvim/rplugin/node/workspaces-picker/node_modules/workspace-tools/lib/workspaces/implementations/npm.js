"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspacesAsync = exports.getWorkspaces = exports.getNpmWorkspacesAsync = exports.getNpmWorkspaces = exports.getWorkspacePackagePathsAsync = exports.getWorkspacePackagePaths = void 0;
const _1 = require(".");
const packageJsonWorkspaces_1 = require("./packageJsonWorkspaces");
function getNpmWorkspaceRoot(cwd) {
    var _a;
    const root = (_a = (0, _1.getWorkspaceManagerAndRoot)(cwd, undefined, "npm")) === null || _a === void 0 ? void 0 : _a.root;
    if (!root) {
        throw new Error("Could not find npm workspace root from " + cwd);
    }
    return root;
}
/** Get package paths for an npm workspace. */
function getWorkspacePackagePaths(cwd) {
    const npmWorkspacesRoot = getNpmWorkspaceRoot(cwd);
    return (0, packageJsonWorkspaces_1.getPackagePathsFromWorkspaceRoot)(npmWorkspacesRoot);
}
exports.getWorkspacePackagePaths = getWorkspacePackagePaths;
/** Get package paths for an npm workspace. */
function getWorkspacePackagePathsAsync(cwd) {
    const npmWorkspacesRoot = getNpmWorkspaceRoot(cwd);
    return (0, packageJsonWorkspaces_1.getPackagePathsFromWorkspaceRootAsync)(npmWorkspacesRoot);
}
exports.getWorkspacePackagePathsAsync = getWorkspacePackagePathsAsync;
/**
 * Get an array with names, paths, and package.json contents for each package in an npm workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
function getNpmWorkspaces(cwd) {
    const npmWorkspacesRoot = getNpmWorkspaceRoot(cwd);
    return (0, packageJsonWorkspaces_1.getWorkspaceInfoFromWorkspaceRoot)(npmWorkspacesRoot);
}
exports.getNpmWorkspaces = getNpmWorkspaces;
exports.getWorkspaces = getNpmWorkspaces;
/**
 * Get an array with names, paths, and package.json contents for each package in an npm workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
function getNpmWorkspacesAsync(cwd) {
    const npmWorkspacesRoot = getNpmWorkspaceRoot(cwd);
    return (0, packageJsonWorkspaces_1.getWorkspaceInfoFromWorkspaceRootAsync)(npmWorkspacesRoot);
}
exports.getNpmWorkspacesAsync = getNpmWorkspacesAsync;
exports.getWorkspacesAsync = getNpmWorkspacesAsync;
//# sourceMappingURL=npm.js.map