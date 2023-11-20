"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getAllPackageJsonFilesAsync = exports._resetPackageJsonFilesCache = exports.getAllPackageJsonFiles = void 0;
const path_1 = __importDefault(require("path"));
const getWorkspacePackagePaths_1 = require("./getWorkspacePackagePaths");
const isCachingEnabled_1 = require("../isCachingEnabled");
const cache = new Map();
/**
 * Get paths to every package.json in the workspace, given a cwd.
 */
function getAllPackageJsonFiles(cwd) {
    if ((0, isCachingEnabled_1.isCachingEnabled)() && cache.has(cwd)) {
        return cache.get(cwd);
    }
    const packageJsonFiles = (0, getWorkspacePackagePaths_1.getWorkspacePackagePaths)(cwd).map((packagePath) => path_1.default.join(packagePath, "package.json"));
    cache.set(cwd, packageJsonFiles);
    return packageJsonFiles;
}
exports.getAllPackageJsonFiles = getAllPackageJsonFiles;
function _resetPackageJsonFilesCache() {
    cache.clear();
}
exports._resetPackageJsonFilesCache = _resetPackageJsonFilesCache;
/**
 * Get paths to every package.json in the workspace, given a cwd.
 */
async function getAllPackageJsonFilesAsync(cwd) {
    if ((0, isCachingEnabled_1.isCachingEnabled)() && cache.has(cwd)) {
        return cache.get(cwd);
    }
    const packageJsonFiles = (await (0, getWorkspacePackagePaths_1.getWorkspacePackagePathsAsync)(cwd)).map((packagePath) => path_1.default.join(packagePath, "package.json"));
    cache.set(cwd, packageJsonFiles);
    return packageJsonFiles;
}
exports.getAllPackageJsonFilesAsync = getAllPackageJsonFilesAsync;
//# sourceMappingURL=getAllPackageJsonFiles.js.map