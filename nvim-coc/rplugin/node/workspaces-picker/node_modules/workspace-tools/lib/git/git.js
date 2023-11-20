"use strict";
//
// Basic git wrappers
//
Object.defineProperty(exports, "__esModule", { value: true });
exports.gitFailFast = exports.git = exports.clearGitObservers = exports.addGitObserver = exports.GitError = void 0;
const child_process_1 = require("child_process");
class GitError extends Error {
    constructor(message, originalError) {
        if (originalError instanceof Error) {
            super(`${message}: ${originalError.message}`);
        }
        else {
            super(message);
        }
        this.originalError = originalError;
    }
}
exports.GitError = GitError;
/**
 * A global maxBuffer override for all git operations.
 * Bumps up the default to 500MB instead of 1MB.
 * Override this value with the `GIT_MAX_BUFFER` environment variable.
 */
const defaultMaxBuffer = process.env.GIT_MAX_BUFFER ? parseInt(process.env.GIT_MAX_BUFFER) : 500 * 1024 * 1024;
const isDebug = !!process.env.GIT_DEBUG;
const observers = [];
let observing;
/**
 * Adds an observer for the git operations, e.g. for testing
 * @returns a function to remove the observer
 */
function addGitObserver(observer) {
    observers.push(observer);
    return () => removeGitObserver(observer);
}
exports.addGitObserver = addGitObserver;
/** Clear all git observers */
function clearGitObservers() {
    observers.splice(0, observers.length);
}
exports.clearGitObservers = clearGitObservers;
/** Remove a git observer */
function removeGitObserver(observer) {
    const index = observers.indexOf(observer);
    if (index > -1) {
        observers.splice(index, 1);
    }
}
/**
 * Runs git command - use this for read-only commands
 */
function git(args, options) {
    isDebug && console.log(`git ${args.join(" ")}`);
    const results = (0, child_process_1.spawnSync)("git", args, { maxBuffer: defaultMaxBuffer, ...options });
    const output = {
        ...results,
        // these may be undefined if stdio: inherit is set
        stderr: (results.stderr || "").toString().trimRight(),
        stdout: (results.stdout || "").toString().trimRight(),
        success: results.status === 0,
    };
    if (isDebug) {
        console.log("exited with code " + results.status);
        output.stdout && console.log("git stdout:\n", output.stdout);
        output.stderr && console.warn("git stderr:\n", output.stderr);
    }
    // notify observers, flipping the observing bit to prevent infinite loops
    if (!observing) {
        observing = true;
        for (const observer of observers) {
            observer(args, output);
        }
        observing = false;
    }
    return output;
}
exports.git = git;
/**
 * Runs git command - use this for commands that make changes to the filesystem
 */
function gitFailFast(args, options) {
    var _a, _b;
    const gitResult = git(args, options);
    if (!gitResult.success) {
        if (!(options === null || options === void 0 ? void 0 : options.noExitCode)) {
            process.exitCode = 1;
        }
        throw new GitError(`CRITICAL ERROR: running git command: git ${args.join(" ")}!
    ${(_a = gitResult.stdout) === null || _a === void 0 ? void 0 : _a.toString().trimRight()}
    ${(_b = gitResult.stderr) === null || _b === void 0 ? void 0 : _b.toString().trimRight()}`);
    }
}
exports.gitFailFast = gitFailFast;
//# sourceMappingURL=git.js.map