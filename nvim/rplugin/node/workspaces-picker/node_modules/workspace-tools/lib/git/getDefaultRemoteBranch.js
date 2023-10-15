"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getDefaultRemoteBranch = void 0;
const getDefaultRemote_1 = require("./getDefaultRemote");
const git_1 = require("./git");
const gitUtilities_1 = require("./gitUtilities");
function getDefaultRemoteBranch(...args) {
    var _a;
    const [branchOrOptions, argsCwd] = args;
    const options = typeof branchOrOptions === "string"
        ? { branch: branchOrOptions, cwd: argsCwd }
        : branchOrOptions;
    const { cwd, branch } = options;
    const defaultRemote = (0, getDefaultRemote_1.getDefaultRemote)(options);
    if (branch) {
        return `${defaultRemote}/${branch}`;
    }
    const showRemote = (0, git_1.git)(["remote", "show", defaultRemote], { cwd });
    let remoteDefaultBranch;
    if (showRemote.success) {
        /**
         * `showRemote.stdout` is something like this:
         *
         * * remote origin
         *   Fetch URL: .../monorepo-upstream
         *   Push  URL: .../monorepo-upstream
         *   HEAD branch: main
         */
        remoteDefaultBranch = (_a = showRemote.stdout
            .split(/\n/)
            .find((line) => line.includes("HEAD branch"))) === null || _a === void 0 ? void 0 : _a.replace(/^\s*HEAD branch:\s+/, "");
    }
    return `${defaultRemote}/${remoteDefaultBranch || (0, gitUtilities_1.getDefaultBranch)(cwd)}`;
}
exports.getDefaultRemoteBranch = getDefaultRemoteBranch;
//# sourceMappingURL=getDefaultRemoteBranch.js.map