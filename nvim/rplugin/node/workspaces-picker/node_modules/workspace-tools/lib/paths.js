"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.isChildOf = exports.findProjectRoot = exports.findPackageRoot = exports.findGitRoot = exports.searchUp = void 0;
const path_1 = __importDefault(require("path"));
const fs_1 = __importDefault(require("fs"));
const getWorkspaceRoot_1 = require("./workspaces/getWorkspaceRoot");
const git_1 = require("./git");
const logging_1 = require("./logging");
/**
 * Starting from `cwd`, searches up the directory hierarchy for `filePath`.
 * If multiple strings are given, searches each directory level for any of them.
 * @returns Full path to the item found, or undefined if not found.
 */
function searchUp(filePath, cwd) {
    const paths = typeof filePath === "string" ? [filePath] : filePath;
    // convert to an absolute path if needed
    cwd = path_1.default.resolve(cwd);
    const root = path_1.default.parse(cwd).root;
    let foundPath;
    while (!foundPath && cwd !== root) {
        foundPath = paths.find((p) => fs_1.default.existsSync(path_1.default.join(cwd, p)));
        if (foundPath) {
            break;
        }
        cwd = path_1.default.dirname(cwd);
    }
    return foundPath ? path_1.default.join(cwd, foundPath) : undefined;
}
exports.searchUp = searchUp;
/**
 * Starting from `cwd`, uses `git rev-parse --show-toplevel` to find the root of the git repo.
 * Throws if `cwd` is not in a Git repository.
 */
function findGitRoot(cwd) {
    const output = (0, git_1.git)(["rev-parse", "--show-toplevel"], { cwd });
    if (!output.success) {
        throw new Error(`Directory "${cwd}" is not in a git repository`);
    }
    return path_1.default.normalize(output.stdout);
}
exports.findGitRoot = findGitRoot;
/**
 * Starting from `cwd`, searches up the directory hierarchy for `package.json`.
 */
function findPackageRoot(cwd) {
    const jsonPath = searchUp("package.json", cwd);
    return jsonPath && path_1.default.dirname(jsonPath);
}
exports.findPackageRoot = findPackageRoot;
/**
 * Starting from `cwd`, searches up the directory hierarchy for the workspace root,
 * falling back to the git root if no workspace is detected.
 */
function findProjectRoot(cwd) {
    let workspaceRoot;
    try {
        workspaceRoot = (0, getWorkspaceRoot_1.getWorkspaceRoot)(cwd);
    }
    catch (err) {
        (0, logging_1.logVerboseWarning)(`Error getting workspace root for ${cwd}`, err);
    }
    if (!workspaceRoot) {
        (0, logging_1.logVerboseWarning)(`Could not find workspace root for ${cwd}. Falling back to git root.`);
    }
    return workspaceRoot || findGitRoot(cwd);
}
exports.findProjectRoot = findProjectRoot;
function isChildOf(child, parent) {
    const relativePath = path_1.default.relative(child, parent);
    return /^[.\/\\]+$/.test(relativePath);
}
exports.isChildOf = isChildOf;
//# sourceMappingURL=paths.js.map