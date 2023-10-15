"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getPackagePathsAsync = exports.getPackagePaths = void 0;
const path_1 = __importDefault(require("path"));
const fast_glob_1 = __importDefault(require("fast-glob"));
const isCachingEnabled_1 = require("./isCachingEnabled");
const packagePathsCache = {};
const globOptions = {
    absolute: true,
    ignore: ["**/node_modules/**", "**/__fixtures__/**"],
    stats: false,
};
/**
 * Given package folder globs (such as those from package.json `workspaces`) and a workspace root
 * directory, get paths to actual package folders.
 */
function getPackagePaths(root, packageGlobs) {
    if ((0, isCachingEnabled_1.isCachingEnabled)() && packagePathsCache[root]) {
        return packagePathsCache[root];
    }
    packagePathsCache[root] = fast_glob_1.default
        .sync(getPackageJsonGlobs(packageGlobs), { cwd: root, ...globOptions })
        .map(getResultPackagePath);
    return packagePathsCache[root];
}
exports.getPackagePaths = getPackagePaths;
/**
 * Given package folder globs (such as those from package.json `workspaces`) and a workspace root
 * directory, get paths to actual package folders.
 */
async function getPackagePathsAsync(root, packageGlobs) {
    if ((0, isCachingEnabled_1.isCachingEnabled)() && packagePathsCache[root]) {
        return packagePathsCache[root];
    }
    packagePathsCache[root] = (await (0, fast_glob_1.default)(getPackageJsonGlobs(packageGlobs), { cwd: root, ...globOptions })).map(getResultPackagePath);
    return packagePathsCache[root];
}
exports.getPackagePathsAsync = getPackagePathsAsync;
function getPackageJsonGlobs(packageGlobs) {
    return packageGlobs.map((glob) => path_1.default.join(glob, "package.json").replace(/\\/g, "/"));
}
function getResultPackagePath(packageJsonPath) {
    const packagePath = path_1.default.dirname(packageJsonPath);
    return path_1.default.sep === "/" ? packagePath : packagePath.replace(/\//g, path_1.default.sep);
}
//# sourceMappingURL=getPackagePaths.js.map