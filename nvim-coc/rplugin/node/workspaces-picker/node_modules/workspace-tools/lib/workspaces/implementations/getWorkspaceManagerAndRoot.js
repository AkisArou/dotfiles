"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspaceManagerAndRoot = exports.getPreferredWorkspaceManager = void 0;
const path_1 = __importDefault(require("path"));
const paths_1 = require("../../paths");
const isCachingEnabled_1 = require("../../isCachingEnabled");
const workspaceCache = new Map();
/**
 * Files indicating the workspace root for each manager.
 *
 * DO NOT REORDER! The order of keys determines the precedence of the files, which is
 * important for cases like lerna where lerna.json and e.g. yarn.lock may both exist.
 */
const managerFiles = {
    // DO NOT REORDER! (see above)
    lerna: "lerna.json",
    rush: "rush.json",
    yarn: "yarn.lock",
    pnpm: "pnpm-workspace.yaml",
    npm: "package-lock.json",
};
/**
 * Get the preferred workspace manager based on `process.env.PREFERRED_WORKSPACE_MANAGER`
 * (if valid).
 */
function getPreferredWorkspaceManager() {
    const preferred = process.env.PREFERRED_WORKSPACE_MANAGER;
    return preferred && managerFiles[preferred] ? preferred : undefined;
}
exports.getPreferredWorkspaceManager = getPreferredWorkspaceManager;
/**
 * Get the workspace manager name and workspace root directory for `cwd`, with caching.
 * Also respects the `process.env.PREFERRED_WORKSPACE_MANAGER` override, provided the relevant
 * manager file exists.
 * @param cwd Directory to search up from
 * @param cache Optional override cache for testing
 * @param preferredManager Optional override manager (if provided, only searches for this manager's file)
 * @returns Workspace manager and root, or undefined if it can't be determined
 */
function getWorkspaceManagerAndRoot(cwd, cache, preferredManager) {
    cache = cache || workspaceCache;
    if ((0, isCachingEnabled_1.isCachingEnabled)() && cache.has(cwd)) {
        return cache.get(cwd);
    }
    preferredManager = preferredManager || getPreferredWorkspaceManager();
    const managerFile = (0, paths_1.searchUp)((preferredManager && managerFiles[preferredManager]) || Object.values(managerFiles), cwd);
    if (managerFile) {
        const managerFileName = path_1.default.basename(managerFile);
        cache.set(cwd, {
            manager: Object.keys(managerFiles).find((name) => managerFiles[name] === managerFileName),
            root: path_1.default.dirname(managerFile),
        });
    }
    else {
        // Avoid searching again if no file was found
        cache.set(cwd, undefined);
    }
    return cache.get(cwd);
}
exports.getWorkspaceManagerAndRoot = getWorkspaceManagerAndRoot;
//# sourceMappingURL=getWorkspaceManagerAndRoot.js.map