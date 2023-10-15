"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspacesAsync = exports.getWorkspaces = exports.getRushWorkspacesAsync = exports.getRushWorkspaces = exports.getWorkspacePackagePaths = exports.getRushWorkspaceRoot = void 0;
const path_1 = __importDefault(require("path"));
const jju_1 = __importDefault(require("jju"));
const fs_1 = __importDefault(require("fs"));
const getWorkspacePackageInfo_1 = require("../getWorkspacePackageInfo");
const logging_1 = require("../../logging");
const getWorkspaceManagerAndRoot_1 = require("./getWorkspaceManagerAndRoot");
/** @deprecated Use getWorkspaceRoot */
function getRushWorkspaceRoot(cwd) {
    var _a;
    const root = (_a = (0, getWorkspaceManagerAndRoot_1.getWorkspaceManagerAndRoot)(cwd, undefined, "rush")) === null || _a === void 0 ? void 0 : _a.root;
    if (!root) {
        throw new Error("Could not find rush workspace root from " + cwd);
    }
    return root;
}
exports.getRushWorkspaceRoot = getRushWorkspaceRoot;
/** Get package paths for a rush workspace. */
function getWorkspacePackagePaths(cwd) {
    try {
        const rushWorkspaceRoot = getRushWorkspaceRoot(cwd);
        const rushJsonPath = path_1.default.join(rushWorkspaceRoot, "rush.json");
        const rushConfig = jju_1.default.parse(fs_1.default.readFileSync(rushJsonPath, "utf-8"));
        const root = path_1.default.dirname(rushJsonPath);
        return rushConfig.projects.map((project) => path_1.default.join(root, project.projectFolder));
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting rush workspace package paths for ${cwd}`, err);
        return [];
    }
}
exports.getWorkspacePackagePaths = getWorkspacePackagePaths;
/**
 * Get an array with names, paths, and package.json contents for each package in a rush workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
function getRushWorkspaces(cwd) {
    try {
        const packagePaths = getWorkspacePackagePaths(cwd);
        return (0, getWorkspacePackageInfo_1.getWorkspacePackageInfo)(packagePaths);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting rush workspaces for ${cwd}`, err);
        return [];
    }
}
exports.getRushWorkspaces = getRushWorkspaces;
exports.getWorkspaces = getRushWorkspaces;
/**
 * Get an array with names, paths, and package.json contents for each package in a rush workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
async function getRushWorkspacesAsync(cwd) {
    try {
        const packagePaths = getWorkspacePackagePaths(cwd);
        return (0, getWorkspacePackageInfo_1.getWorkspacePackageInfoAsync)(packagePaths);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting rush workspaces for ${cwd}`, err);
        return [];
    }
}
exports.getRushWorkspacesAsync = getRushWorkspacesAsync;
exports.getWorkspacesAsync = getRushWorkspacesAsync;
//# sourceMappingURL=rush.js.map