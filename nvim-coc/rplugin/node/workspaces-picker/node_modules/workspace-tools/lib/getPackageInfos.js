"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getPackageInfosAsync = exports.getPackageInfos = void 0;
const fs_1 = __importDefault(require("fs"));
const path_1 = __importDefault(require("path"));
const infoFromPackageJson_1 = require("./infoFromPackageJson");
const getWorkspaces_1 = require("./workspaces/getWorkspaces");
function getPackageInfos(cwd) {
    const packageInfos = {};
    const workspacePackages = (0, getWorkspaces_1.getWorkspaces)(cwd);
    if (workspacePackages.length) {
        for (const pkg of workspacePackages) {
            packageInfos[pkg.name] = pkg.packageJson;
        }
    }
    else {
        const rootInfo = tryReadRootPackageJson(cwd);
        if (rootInfo) {
            packageInfos[rootInfo.name] = rootInfo;
        }
    }
    return packageInfos;
}
exports.getPackageInfos = getPackageInfos;
async function getPackageInfosAsync(cwd) {
    const packageInfos = {};
    const workspacePackages = await (0, getWorkspaces_1.getWorkspacesAsync)(cwd);
    if (workspacePackages.length) {
        for (const pkg of workspacePackages) {
            packageInfos[pkg.name] = pkg.packageJson;
        }
    }
    else {
        const rootInfo = tryReadRootPackageJson(cwd);
        if (rootInfo) {
            packageInfos[rootInfo.name] = rootInfo;
        }
    }
    return packageInfos;
}
exports.getPackageInfosAsync = getPackageInfosAsync;
function tryReadRootPackageJson(cwd) {
    var _a;
    const packageJsonPath = path_1.default.join(cwd, "package.json");
    if (fs_1.default.existsSync(packageJsonPath)) {
        try {
            const packageJson = JSON.parse(fs_1.default.readFileSync(packageJsonPath, "utf-8"));
            return (0, infoFromPackageJson_1.infoFromPackageJson)(packageJson, packageJsonPath);
        }
        catch (e) {
            throw new Error(`Invalid package.json file detected ${packageJsonPath}: ${((_a = e) === null || _a === void 0 ? void 0 : _a.message) || e}`);
        }
    }
}
//# sourceMappingURL=getPackageInfos.js.map