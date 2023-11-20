"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspacesAsync = exports.getWorkspaces = exports.getYarnWorkspacesAsync = exports.getYarnWorkspaces = exports.getWorkspacePackagePathsAsync = exports.getWorkspacePackagePaths = exports.getYarnWorkspaceRoot = void 0;
const _1 = require(".");
const packageJsonWorkspaces_1 = require("./packageJsonWorkspaces");
/** @deprecated Use `getWorkspaceRoot` */
function getYarnWorkspaceRoot(cwd) {
    var _a;
    const root = (_a = (0, _1.getWorkspaceManagerAndRoot)(cwd, undefined, "yarn")) === null || _a === void 0 ? void 0 : _a.root;
    if (!root) {
        throw new Error("Could not find yarn workspace root from " + cwd);
    }
    return root;
}
exports.getYarnWorkspaceRoot = getYarnWorkspaceRoot;
/** Get package paths for a yarn workspace. */
function getWorkspacePackagePaths(cwd) {
    const yarnWorkspacesRoot = getYarnWorkspaceRoot(cwd);
    return (0, packageJsonWorkspaces_1.getPackagePathsFromWorkspaceRoot)(yarnWorkspacesRoot);
}
exports.getWorkspacePackagePaths = getWorkspacePackagePaths;
/** Get package paths for a yarn workspace. */
function getWorkspacePackagePathsAsync(cwd) {
    const yarnWorkspacesRoot = getYarnWorkspaceRoot(cwd);
    return (0, packageJsonWorkspaces_1.getPackagePathsFromWorkspaceRootAsync)(yarnWorkspacesRoot);
}
exports.getWorkspacePackagePathsAsync = getWorkspacePackagePathsAsync;
/**
 * Get an array with names, paths, and package.json contents for each package in a yarn workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
function getYarnWorkspaces(cwd) {
    const yarnWorkspacesRoot = getYarnWorkspaceRoot(cwd);
    return (0, packageJsonWorkspaces_1.getWorkspaceInfoFromWorkspaceRoot)(yarnWorkspacesRoot);
}
exports.getYarnWorkspaces = getYarnWorkspaces;
exports.getWorkspaces = getYarnWorkspaces;
/**
 * Get an array with names, paths, and package.json contents for each package in a yarn workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
function getYarnWorkspacesAsync(cwd) {
    const yarnWorkspacesRoot = getYarnWorkspaceRoot(cwd);
    return (0, packageJsonWorkspaces_1.getWorkspaceInfoFromWorkspaceRootAsync)(yarnWorkspacesRoot);
}
exports.getYarnWorkspacesAsync = getYarnWorkspacesAsync;
exports.getWorkspacesAsync = getYarnWorkspacesAsync;
//# sourceMappingURL=yarn.js.map