"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getChangedPackages = exports.getChangedPackagesBetweenRefs = void 0;
const git_1 = require("../git");
const getPackagesByFiles_1 = require("./getPackagesByFiles");
/**
 * Finds all packages that had been changed between two refs in the repo under cwd
 *
 * executes a `git diff $fromRef...$toRef` to get changes given a merge-base
 *
 * further explanation with the three dots:
 *
 * ```txt
 * git diff [--options] <commit>...<commit> [--] [<path>...]
 *
 *    This form is to view the changes on the branch containing and up to
 *    the second <commit>, starting at a common ancestor of both
 *    <commit>. "git diff A...B" is equivalent to "git diff
 *    $(git-merge-base A B) B". You can omit any one of <commit>, which
 *    has the same effect as using HEAD instead.
 * ```
 *
 * @returns string[] of package names that have changed
 */
function getChangedPackagesBetweenRefs(cwd, fromRef, toRef = "", ignoreGlobs = []) {
    let changes = [
        ...new Set([
            ...((0, git_1.getUntrackedChanges)(cwd) || []),
            ...((0, git_1.getUnstagedChanges)(cwd) || []),
            ...((0, git_1.getChangesBetweenRefs)(fromRef, toRef, [], "", cwd) || []),
            ...((0, git_1.getStagedChanges)(cwd) || []),
        ]),
    ];
    return (0, getPackagesByFiles_1.getPackagesByFiles)(cwd, changes, ignoreGlobs, true);
}
exports.getChangedPackagesBetweenRefs = getChangedPackagesBetweenRefs;
/**
 * Finds all packages that had been changed in the repo under cwd
 *
 * executes a `git diff $Target...` to get changes given a merge-base
 *
 * further explanation with the three dots:
 *
 * ```txt
 * git diff [--options] <commit>...<commit> [--] [<path>...]
 *
 *    This form is to view the changes on the branch containing and up to
 *    the second <commit>, starting at a common ancestor of both
 *    <commit>. "git diff A...B" is equivalent to "git diff
 *    $(git-merge-base A B) B". You can omit any one of <commit>, which
 *    has the same effect as using HEAD instead.
 * ```
 *
 * @returns string[] of package names that have changed
 */
function getChangedPackages(cwd, target, ignoreGlobs = []) {
    const targetBranch = target || (0, git_1.getDefaultRemoteBranch)({ cwd });
    let changes = [
        ...new Set([
            ...((0, git_1.getUntrackedChanges)(cwd) || []),
            ...((0, git_1.getUnstagedChanges)(cwd) || []),
            ...((0, git_1.getBranchChanges)(targetBranch, cwd) || []),
            ...((0, git_1.getStagedChanges)(cwd) || []),
        ]),
    ];
    return (0, getPackagesByFiles_1.getPackagesByFiles)(cwd, changes, ignoreGlobs, true);
}
exports.getChangedPackages = getChangedPackages;
//# sourceMappingURL=getChangedPackages.js.map