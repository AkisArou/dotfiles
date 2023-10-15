"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspacesAsync = exports.getWorkspaces = exports.getLernaWorkspacesAsync = exports.getLernaWorkspaces = exports.getWorkspacePackagePaths = void 0;
const fs_1 = __importDefault(require("fs"));
const jju_1 = __importDefault(require("jju"));
const path_1 = __importDefault(require("path"));
const getPackagePaths_1 = require("../../getPackagePaths");
const getWorkspacePackageInfo_1 = require("../getWorkspacePackageInfo");
const logging_1 = require("../../logging");
const getWorkspaceManagerAndRoot_1 = require("./getWorkspaceManagerAndRoot");
function getLernaWorkspaceRoot(cwd) {
    var _a;
    const root = (_a = (0, getWorkspaceManagerAndRoot_1.getWorkspaceManagerAndRoot)(cwd, undefined, "lerna")) === null || _a === void 0 ? void 0 : _a.root;
    if (!root) {
        throw new Error("Could not find lerna workspace root from " + cwd);
    }
    return root;
}
/** Get package paths for a lerna workspace. */
function getWorkspacePackagePaths(cwd) {
    try {
        const lernaWorkspaceRoot = getLernaWorkspaceRoot(cwd);
        const lernaJsonPath = path_1.default.join(lernaWorkspaceRoot, "lerna.json");
        const lernaConfig = jju_1.default.parse(fs_1.default.readFileSync(lernaJsonPath, "utf-8"));
        return (0, getPackagePaths_1.getPackagePaths)(lernaWorkspaceRoot, lernaConfig.packages);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting lerna workspace package paths for ${cwd}`, err);
        return [];
    }
}
exports.getWorkspacePackagePaths = getWorkspacePackagePaths;
/**
 * Get an array with names, paths, and package.json contents for each package in a lerna workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
function getLernaWorkspaces(cwd) {
    try {
        const packagePaths = getWorkspacePackagePaths(cwd);
        return (0, getWorkspacePackageInfo_1.getWorkspacePackageInfo)(packagePaths);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting lerna workspaces for ${cwd}`, err);
        return [];
    }
}
exports.getLernaWorkspaces = getLernaWorkspaces;
exports.getWorkspaces = getLernaWorkspaces;
/**
 * Get an array with names, paths, and package.json contents for each package in a lerna workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
async function getLernaWorkspacesAsync(cwd) {
    try {
        const packagePaths = getWorkspacePackagePaths(cwd);
        return (0, getWorkspacePackageInfo_1.getWorkspacePackageInfoAsync)(packagePaths);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting lerna workspaces for ${cwd}`, err);
        return [];
    }
}
exports.getLernaWorkspacesAsync = getLernaWorkspacesAsync;
exports.getWorkspacesAsync = getLernaWorkspacesAsync;
//# sourceMappingURL=lerna.js.map