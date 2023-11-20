"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspacesAsync = exports.getWorkspaces = exports.getPnpmWorkspacesAsync = exports.getPnpmWorkspaces = exports.getWorkspacePackagePaths = exports.getPnpmWorkspaceRoot = void 0;
const path_1 = __importDefault(require("path"));
const getPackagePaths_1 = require("../../getPackagePaths");
const getWorkspacePackageInfo_1 = require("../getWorkspacePackageInfo");
const readYaml_1 = require("../../lockfile/readYaml");
const logging_1 = require("../../logging");
const getWorkspaceManagerAndRoot_1 = require("./getWorkspaceManagerAndRoot");
/** @deprecated Use `getWorkspaceRoot` */
function getPnpmWorkspaceRoot(cwd) {
    var _a;
    const root = (_a = (0, getWorkspaceManagerAndRoot_1.getWorkspaceManagerAndRoot)(cwd, undefined, "pnpm")) === null || _a === void 0 ? void 0 : _a.root;
    if (!root) {
        throw new Error("Could not find pnpm workspace root from " + cwd);
    }
    return root;
}
exports.getPnpmWorkspaceRoot = getPnpmWorkspaceRoot;
/** Get package paths for a pnpm workspace. */
function getWorkspacePackagePaths(cwd) {
    try {
        const pnpmWorkspacesRoot = getPnpmWorkspaceRoot(cwd);
        const pnpmWorkspacesFile = path_1.default.join(pnpmWorkspacesRoot, "pnpm-workspace.yaml");
        const pnpmWorkspaces = (0, readYaml_1.readYaml)(pnpmWorkspacesFile);
        return (0, getPackagePaths_1.getPackagePaths)(pnpmWorkspacesRoot, pnpmWorkspaces.packages);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting pnpm workspace package paths for ${cwd}`, err);
        return [];
    }
}
exports.getWorkspacePackagePaths = getWorkspacePackagePaths;
/**
 * Get an array with names, paths, and package.json contents for each package in a pnpm workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
function getPnpmWorkspaces(cwd) {
    try {
        const packagePaths = getWorkspacePackagePaths(cwd);
        return (0, getWorkspacePackageInfo_1.getWorkspacePackageInfo)(packagePaths);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting pnpm workspaces for ${cwd}`, err);
        return [];
    }
}
exports.getPnpmWorkspaces = getPnpmWorkspaces;
exports.getWorkspaces = getPnpmWorkspaces;
/**
 * Get an array with names, paths, and package.json contents for each package in a pnpm workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
async function getPnpmWorkspacesAsync(cwd) {
    try {
        const packagePaths = getWorkspacePackagePaths(cwd);
        return (0, getWorkspacePackageInfo_1.getWorkspacePackageInfoAsync)(packagePaths);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting pnpm workspaces for ${cwd}`, err);
        return [];
    }
}
exports.getPnpmWorkspacesAsync = getPnpmWorkspacesAsync;
exports.getWorkspacesAsync = getPnpmWorkspacesAsync;
//# sourceMappingURL=pnpm.js.map