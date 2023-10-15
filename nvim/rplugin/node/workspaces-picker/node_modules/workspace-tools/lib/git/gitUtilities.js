"use strict";
//
// Assorted other git utilities
// (could be split into separate files later if desired)
//
Object.defineProperty(exports, "__esModule", { value: true });
exports.listAllTrackedFiles = exports.getDefaultBranch = exports.parseRemoteBranch = exports.getRemoteBranch = exports.getParentBranch = exports.revertLocalChanges = exports.stageAndCommit = exports.commit = exports.stage = exports.init = exports.getFileAddedHash = exports.getCurrentHash = exports.getShortBranchName = exports.getFullBranchRef = exports.getBranchName = exports.getUserEmail = exports.getRecentCommitMessages = exports.getStagedChanges = exports.getChangesBetweenRefs = exports.getBranchChanges = exports.getChanges = exports.getUnstagedChanges = exports.fetchRemoteBranch = exports.fetchRemote = exports.getUntrackedChanges = void 0;
const git_1 = require("./git");
function getUntrackedChanges(cwd) {
    try {
        return processGitOutput((0, git_1.git)(["ls-files", "--others", "--exclude-standard"], { cwd }));
    }
    catch (e) {
        throw new git_1.GitError(`Cannot gather information about untracked changes`, e);
    }
}
exports.getUntrackedChanges = getUntrackedChanges;
function fetchRemote(remote, cwd) {
    const results = (0, git_1.git)(["fetch", "--", remote], { cwd });
    if (!results.success) {
        throw new git_1.GitError(`Cannot fetch remote "${remote}"`);
    }
}
exports.fetchRemote = fetchRemote;
function fetchRemoteBranch(remote, remoteBranch, cwd) {
    const results = (0, git_1.git)(["fetch", "--", remote, remoteBranch], { cwd });
    if (!results.success) {
        throw new git_1.GitError(`Cannot fetch branch "${remoteBranch}" from remote "${remote}"`);
    }
}
exports.fetchRemoteBranch = fetchRemoteBranch;
/**
 * Gets all the changes that have not been staged yet
 * @param cwd
 */
function getUnstagedChanges(cwd) {
    try {
        return processGitOutput((0, git_1.git)(["--no-pager", "diff", "--name-only", "--relative"], { cwd }));
    }
    catch (e) {
        throw new git_1.GitError(`Cannot gather information about unstaged changes`, e);
    }
}
exports.getUnstagedChanges = getUnstagedChanges;
function getChanges(branch, cwd) {
    try {
        return processGitOutput((0, git_1.git)(["--no-pager", "diff", "--relative", "--name-only", branch + "..."], { cwd }));
    }
    catch (e) {
        throw new git_1.GitError(`Cannot gather information about changes`, e);
    }
}
exports.getChanges = getChanges;
/**
 * Gets all the changes between the branch and the merge-base
 */
function getBranchChanges(branch, cwd) {
    return getChangesBetweenRefs(branch, "", [], "", cwd);
}
exports.getBranchChanges = getBranchChanges;
function getChangesBetweenRefs(fromRef, toRef, options, pattern, cwd) {
    try {
        return processGitOutput((0, git_1.git)([
            "--no-pager",
            "diff",
            "--name-only",
            "--relative",
            ...options,
            `${fromRef}...${toRef}`,
            ...(pattern ? ["--", pattern] : []),
        ], { cwd }));
    }
    catch (e) {
        throw new git_1.GitError(`Cannot gather information about change between refs changes (${fromRef} to ${toRef})`, e);
    }
}
exports.getChangesBetweenRefs = getChangesBetweenRefs;
function getStagedChanges(cwd) {
    try {
        return processGitOutput((0, git_1.git)(["--no-pager", "diff", "--relative", "--staged", "--name-only"], { cwd }));
    }
    catch (e) {
        throw new git_1.GitError(`Cannot gather information about staged changes`, e);
    }
}
exports.getStagedChanges = getStagedChanges;
function getRecentCommitMessages(branch, cwd) {
    try {
        const results = (0, git_1.git)(["log", "--decorate", "--pretty=format:%s", `${branch}..HEAD`], { cwd });
        if (!results.success) {
            return [];
        }
        return results.stdout
            .trim()
            .split(/\n/)
            .map((line) => line.trim());
    }
    catch (e) {
        throw new git_1.GitError(`Cannot gather information about recent commits`, e);
    }
}
exports.getRecentCommitMessages = getRecentCommitMessages;
function getUserEmail(cwd) {
    try {
        const results = (0, git_1.git)(["config", "user.email"], { cwd });
        return results.success ? results.stdout : null;
    }
    catch (e) {
        throw new git_1.GitError(`Cannot gather information about user.email`, e);
    }
}
exports.getUserEmail = getUserEmail;
function getBranchName(cwd) {
    try {
        const results = (0, git_1.git)(["rev-parse", "--abbrev-ref", "HEAD"], { cwd });
        return results.success ? results.stdout : null;
    }
    catch (e) {
        throw new git_1.GitError(`Cannot get branch name`, e);
    }
}
exports.getBranchName = getBranchName;
function getFullBranchRef(branch, cwd) {
    const showRefResults = (0, git_1.git)(["show-ref", "--heads", branch], { cwd });
    return showRefResults.success ? showRefResults.stdout.split(" ")[1] : null;
}
exports.getFullBranchRef = getFullBranchRef;
function getShortBranchName(fullBranchRef, cwd) {
    const showRefResults = (0, git_1.git)(["name-rev", "--name-only", fullBranchRef], {
        cwd,
    });
    return showRefResults.success ? showRefResults.stdout : null;
}
exports.getShortBranchName = getShortBranchName;
function getCurrentHash(cwd) {
    try {
        const results = (0, git_1.git)(["rev-parse", "HEAD"], { cwd });
        return results.success ? results.stdout : null;
    }
    catch (e) {
        throw new git_1.GitError(`Cannot get current git hash`, e);
    }
}
exports.getCurrentHash = getCurrentHash;
/**
 * Get the commit hash in which the file was first added.
 */
function getFileAddedHash(filename, cwd) {
    const results = (0, git_1.git)(["rev-list", "HEAD", filename], { cwd });
    if (results.success) {
        return results.stdout.trim().split("\n").slice(-1)[0];
    }
    return undefined;
}
exports.getFileAddedHash = getFileAddedHash;
function init(cwd, email, username) {
    (0, git_1.git)(["init"], { cwd });
    const configLines = (0, git_1.git)(["config", "--list"], { cwd }).stdout.split("\n");
    if (!configLines.find((line) => line.includes("user.name"))) {
        if (!username) {
            throw new git_1.GitError("must include a username when initializing git repo");
        }
        (0, git_1.git)(["config", "user.name", username], { cwd });
    }
    if (!configLines.find((line) => line.includes("user.email"))) {
        if (!email) {
            throw new Error("must include a email when initializing git repo");
        }
        (0, git_1.git)(["config", "user.email", email], { cwd });
    }
}
exports.init = init;
function stage(patterns, cwd) {
    try {
        patterns.forEach((pattern) => {
            (0, git_1.git)(["add", pattern], { cwd });
        });
    }
    catch (e) {
        throw new git_1.GitError(`Cannot stage changes`, e);
    }
}
exports.stage = stage;
function commit(message, cwd, options = []) {
    try {
        const commitResults = (0, git_1.git)(["commit", "-m", message, ...options], { cwd });
        if (!commitResults.success) {
            throw new Error(`Cannot commit changes: ${commitResults.stdout} ${commitResults.stderr}`);
        }
    }
    catch (e) {
        throw new git_1.GitError(`Cannot commit changes`, e);
    }
}
exports.commit = commit;
function stageAndCommit(patterns, message, cwd, commitOptions = []) {
    stage(patterns, cwd);
    commit(message, cwd, commitOptions);
}
exports.stageAndCommit = stageAndCommit;
function revertLocalChanges(cwd) {
    const stash = `workspace-tools_${new Date().getTime()}`;
    (0, git_1.git)(["stash", "push", "-u", "-m", stash], { cwd });
    const results = (0, git_1.git)(["stash", "list"]);
    if (results.success) {
        const lines = results.stdout.split(/\n/);
        const foundLine = lines.find((line) => line.includes(stash));
        if (foundLine) {
            const matched = foundLine.match(/^[^:]+/);
            if (matched) {
                (0, git_1.git)(["stash", "drop", matched[0]]);
                return true;
            }
        }
    }
    return false;
}
exports.revertLocalChanges = revertLocalChanges;
function getParentBranch(cwd) {
    const branchName = getBranchName(cwd);
    if (!branchName || branchName === "HEAD") {
        return null;
    }
    const showBranchResult = (0, git_1.git)(["show-branch", "-a"], { cwd });
    if (showBranchResult.success) {
        const showBranchLines = showBranchResult.stdout.split(/\n/);
        const parentLine = showBranchLines.find((line) => line.includes("*") && !line.includes(branchName) && !line.includes("publish_"));
        const matched = parentLine === null || parentLine === void 0 ? void 0 : parentLine.match(/\[(.*)\]/);
        return matched ? matched[1] : null;
    }
    return null;
}
exports.getParentBranch = getParentBranch;
function getRemoteBranch(branch, cwd) {
    const results = (0, git_1.git)(["rev-parse", "--abbrev-ref", "--symbolic-full-name", `${branch}@\{u\}`], { cwd });
    if (results.success) {
        return results.stdout.trim();
    }
    return null;
}
exports.getRemoteBranch = getRemoteBranch;
function parseRemoteBranch(branch) {
    const firstSlashPos = branch.indexOf("/", 0);
    const remote = branch.substring(0, firstSlashPos);
    const remoteBranch = branch.substring(firstSlashPos + 1);
    return {
        remote,
        remoteBranch,
    };
}
exports.parseRemoteBranch = parseRemoteBranch;
/**
 * Gets the default branch based on `git config init.defaultBranch`, falling back to `master`.
 */
function getDefaultBranch(cwd) {
    const result = (0, git_1.git)(["config", "init.defaultBranch"], { cwd });
    // Default to the legacy 'master' for backwards compat and old git clients
    return result.success ? result.stdout.trim() : "master";
}
exports.getDefaultBranch = getDefaultBranch;
function listAllTrackedFiles(patterns, cwd) {
    const results = (0, git_1.git)(["ls-files", ...patterns], { cwd });
    return results.success && results.stdout.trim() ? results.stdout.trim().split(/\n/) : [];
}
exports.listAllTrackedFiles = listAllTrackedFiles;
function processGitOutput(output) {
    if (!output.success) {
        return [];
    }
    return output.stdout
        .split(/\n/)
        .map((line) => line.trim())
        .filter((line) => !!line && !line.includes("node_modules"));
}
//# sourceMappingURL=gitUtilities.js.map