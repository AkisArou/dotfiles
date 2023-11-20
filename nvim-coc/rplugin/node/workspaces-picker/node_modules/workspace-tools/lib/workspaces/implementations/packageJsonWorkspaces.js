"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspaceInfoFromWorkspaceRootAsync = exports.getWorkspaceInfoFromWorkspaceRoot = exports.getPackagePathsFromWorkspaceRootAsync = exports.getPackagePathsFromWorkspaceRoot = void 0;
const fs_1 = __importDefault(require("fs"));
const path_1 = __importDefault(require("path"));
const getPackagePaths_1 = require("../../getPackagePaths");
const getWorkspacePackageInfo_1 = require("../getWorkspacePackageInfo");
const logging_1 = require("../../logging");
/**
 * Read the workspace root package.json and get the list of package globs from its `workspaces` property.
 */
function getPackages(packageJsonWorkspacesRoot) {
    const packageJsonFile = path_1.default.join(packageJsonWorkspacesRoot, "package.json");
    let packageJson;
    try {
        packageJson = JSON.parse(fs_1.default.readFileSync(packageJsonFile, "utf-8"));
    }
    catch (e) {
        throw new Error("Could not load package.json from workspaces root");
    }
    const { workspaces } = packageJson;
    if (Array.isArray(workspaces)) {
        return workspaces;
    }
    if (!(workspaces === null || workspaces === void 0 ? void 0 : workspaces.packages)) {
        throw new Error("Could not find a workspaces object in package.json (expected if this is not a monorepo)");
    }
    return workspaces.packages;
}
function getPackagePathsFromWorkspaceRoot(packageJsonWorkspacesRoot) {
    try {
        const packageGlobs = getPackages(packageJsonWorkspacesRoot);
        return packageGlobs ? (0, getPackagePaths_1.getPackagePaths)(packageJsonWorkspacesRoot, packageGlobs) : [];
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting package paths for ${packageJsonWorkspacesRoot}`, err);
        return [];
    }
}
exports.getPackagePathsFromWorkspaceRoot = getPackagePathsFromWorkspaceRoot;
/**
 * Get an array with names, paths, and package.json contents for each package in an npm/yarn workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
async function getPackagePathsFromWorkspaceRootAsync(packageJsonWorkspacesRoot) {
    try {
        const packageGlobs = getPackages(packageJsonWorkspacesRoot);
        return packageGlobs ? (0, getPackagePaths_1.getPackagePathsAsync)(packageJsonWorkspacesRoot, packageGlobs) : [];
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting package paths for ${packageJsonWorkspacesRoot}`, err);
        return [];
    }
}
exports.getPackagePathsFromWorkspaceRootAsync = getPackagePathsFromWorkspaceRootAsync;
/**
 * Get an array with names, paths, and package.json contents for each package in an npm/yarn workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
function getWorkspaceInfoFromWorkspaceRoot(packageJsonWorkspacesRoot) {
    try {
        const packagePaths = getPackagePathsFromWorkspaceRoot(packageJsonWorkspacesRoot);
        return (0, getWorkspacePackageInfo_1.getWorkspacePackageInfo)(packagePaths);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting workspace info for ${packageJsonWorkspacesRoot}`, err);
        return [];
    }
}
exports.getWorkspaceInfoFromWorkspaceRoot = getWorkspaceInfoFromWorkspaceRoot;
/**
 * Get an array with names, paths, and package.json contents for each package in an npm/yarn workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
async function getWorkspaceInfoFromWorkspaceRootAsync(packageJsonWorkspacesRoot) {
    try {
        const packagePaths = await getPackagePathsFromWorkspaceRootAsync(packageJsonWorkspacesRoot);
        return (0, getWorkspacePackageInfo_1.getWorkspacePackageInfoAsync)(packagePaths);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting workspace info for ${packageJsonWorkspacesRoot}`, err);
        return [];
    }
}
exports.getWorkspaceInfoFromWorkspaceRootAsync = getWorkspaceInfoFromWorkspaceRootAsync;
//# sourceMappingURL=packageJsonWorkspaces.js.map