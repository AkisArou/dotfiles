"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getDefaultRemote = void 0;
const fs_1 = __importDefault(require("fs"));
const path_1 = __importDefault(require("path"));
const paths_1 = require("../paths");
const getRepositoryName_1 = require("./getRepositoryName");
const git_1 = require("./git");
function getDefaultRemote(cwdOrOptions) {
    const options = typeof cwdOrOptions === "string" ? { cwd: cwdOrOptions } : cwdOrOptions;
    const { cwd, strict, verbose } = options;
    const log = (message) => verbose && console.log(message);
    const logOrThrow = (message) => {
        if (strict) {
            throw new Error(message);
        }
        log(message);
    };
    const gitRoot = (0, paths_1.findGitRoot)(cwd);
    let packageJson = {};
    const packageJsonPath = path_1.default.join(gitRoot, "package.json");
    try {
        packageJson = JSON.parse(fs_1.default.readFileSync(packageJsonPath, "utf8").trim());
    }
    catch (e) {
        logOrThrow(`Could not read "${packageJsonPath}"`);
    }
    const { repository } = packageJson;
    const repositoryUrl = typeof repository === "string" ? repository : (repository && repository.url) || "";
    if (!repositoryUrl) {
        // This is always logged because it's strongly recommended to fix
        console.log(`Valid "repository" key not found in "${packageJsonPath}". Consider adding this info for more accurate git remote detection.`);
    }
    /** Repository full name (owner and repo name) specified in package.json */
    const repositoryName = (0, getRepositoryName_1.getRepositoryName)(repositoryUrl);
    const remotesResult = (0, git_1.git)(["remote", "-v"], { cwd });
    if (!remotesResult.success) {
        logOrThrow(`Could not determine available git remotes under "${cwd}"`);
    }
    /** Mapping from remote URL to full name (owner and repo name) */
    const remotes = {};
    remotesResult.stdout.split("\n").forEach((line) => {
        const [remoteName, remoteUrl] = line.split(/\s+/);
        const remoteRepoName = (0, getRepositoryName_1.getRepositoryName)(remoteUrl);
        if (remoteRepoName) {
            remotes[remoteRepoName] = remoteName;
        }
    });
    if (repositoryName) {
        // If the repository name was found in package.json, check for a matching remote
        if (remotes[repositoryName]) {
            return remotes[repositoryName];
        }
        // If `strict` is true, and repositoryName is found, there MUST be a matching remote
        logOrThrow(`Could not find remote pointing to repository "${repositoryName}".`);
    }
    // Default to upstream or origin if available, or the first remote otherwise
    const allRemoteNames = Object.values(remotes);
    const fallbacks = ["upstream", "origin", ...allRemoteNames];
    for (const fallback of fallbacks) {
        if (allRemoteNames.includes(fallback)) {
            log(`Default to remote "${fallback}"`);
            return fallback;
        }
    }
    // If we get here, no git remotes were found. This should probably always be an error (since
    // subsequent operations which require a remote likely won't work), but to match old behavior,
    // still default to "origin" unless `strict` is true.
    logOrThrow(`Could not find any remotes in git repo at "${gitRoot}".`);
    log(`Assuming default remote "origin".`);
    return "origin";
}
exports.getDefaultRemote = getDefaultRemote;
//# sourceMappingURL=getDefaultRemote.js.map