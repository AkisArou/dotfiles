"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspacePackageInfoAsync = exports.getWorkspacePackageInfo = void 0;
const path_1 = __importDefault(require("path"));
const fs_1 = __importDefault(require("fs"));
const promises_1 = __importDefault(require("fs/promises"));
const logging_1 = require("../logging");
const infoFromPackageJson_1 = require("../infoFromPackageJson");
/**
 * Get an array with names, paths, and package.json contents for each of the given package paths
 * within a workspace.
 *
 * This is an internal helper used by `getWorkspaces` implementations for different managers.
 * (See `../getWorkspaces` for why it's named this way.)
 * @param packagePaths Paths to packages within a workspace
 * @returns Array of workspace package infos
 * @internal
 */
function getWorkspacePackageInfo(packagePaths) {
    if (!packagePaths) {
        return [];
    }
    return packagePaths
        .map((workspacePath) => {
        let packageJson;
        const packageJsonPath = path_1.default.join(workspacePath, "package.json");
        try {
            packageJson = JSON.parse(fs_1.default.readFileSync(packageJsonPath, "utf-8"));
        }
        catch (err) {
            (0, logging_1.logVerboseWarning)(`Error reading or parsing ${packageJsonPath} while getting workspace package info`, err);
            return null;
        }
        return {
            name: packageJson.name,
            path: workspacePath,
            packageJson: (0, infoFromPackageJson_1.infoFromPackageJson)(packageJson, packageJsonPath),
        };
    })
        .filter(Boolean);
}
exports.getWorkspacePackageInfo = getWorkspacePackageInfo;
/**
 * Get an array with names, paths, and package.json contents for each of the given package paths
 * within a workspace.
 *
 * This is an internal helper used by `getWorkspaces` implementations for different managers.
 * (See `../getWorkspaces` for why it's named this way.)
 * @param packagePaths Paths to packages within a workspace
 * @returns Array of workspace package infos
 * @internal
 */
async function getWorkspacePackageInfoAsync(packagePaths) {
    if (!packagePaths) {
        return [];
    }
    const workspacePkgPromises = packagePaths.map(async (workspacePath) => {
        const packageJsonPath = path_1.default.join(workspacePath, "package.json");
        try {
            const packageJson = JSON.parse(await promises_1.default.readFile(packageJsonPath, "utf-8"));
            return {
                name: packageJson.name,
                path: workspacePath,
                packageJson: (0, infoFromPackageJson_1.infoFromPackageJson)(packageJson, packageJsonPath),
            };
        }
        catch (err) {
            (0, logging_1.logVerboseWarning)(`Error reading or parsing ${packageJsonPath} while getting workspace package info`, err);
            return null;
        }
    });
    return (await Promise.all(workspacePkgPromises)).filter(Boolean);
}
exports.getWorkspacePackageInfoAsync = getWorkspacePackageInfoAsync;
//# sourceMappingURL=getWorkspacePackageInfo.js.map