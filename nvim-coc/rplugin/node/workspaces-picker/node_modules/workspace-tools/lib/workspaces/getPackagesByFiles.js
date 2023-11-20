"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getPackagesByFiles = void 0;
const micromatch_1 = __importDefault(require("micromatch"));
const path_1 = __importDefault(require("path"));
const getWorkspaces_1 = require("./getWorkspaces");
/**
 * Given a list of files, finds all packages names that contain those files
 *
 * @param workspaceRoot - The root of the workspace
 * @param files - files to search for
 * @param ignoreGlobs - glob patterns to ignore
 * @param returnAllPackagesOnNoMatch - if true, will return all packages if no matches are found
 * @returns package names that have changed
 */
function getPackagesByFiles(workspaceRoot, files, ignoreGlobs = [], returnAllPackagesOnNoMatch = false) {
    const workspaceInfo = (0, getWorkspaces_1.getWorkspaces)(workspaceRoot);
    const ignoreSet = new Set((0, micromatch_1.default)(files, ignoreGlobs));
    files = files.filter((change) => !ignoreSet.has(change));
    const packages = new Set();
    for (const file of files) {
        const candidates = workspaceInfo.filter((pkgPath) => file.indexOf(path_1.default.relative(workspaceRoot, pkgPath.path).replace(/\\/g, "/")) === 0);
        if (candidates && candidates.length > 0) {
            const found = candidates.reduce((found, item) => {
                return found.path.length > item.path.length ? found : item;
            }, candidates[0]);
            packages.add(found.name);
        }
        else if (returnAllPackagesOnNoMatch) {
            return workspaceInfo.map((pkg) => pkg.name);
        }
    }
    return [...packages];
}
exports.getPackagesByFiles = getPackagesByFiles;
//# sourceMappingURL=getPackagesByFiles.js.map