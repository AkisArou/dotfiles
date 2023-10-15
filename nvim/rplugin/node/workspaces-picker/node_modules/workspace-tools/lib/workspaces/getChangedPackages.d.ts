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
export declare function getChangedPackagesBetweenRefs(cwd: string, fromRef: string, toRef?: string, ignoreGlobs?: string[]): string[];
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
export declare function getChangedPackages(cwd: string, target: string | undefined, ignoreGlobs?: string[]): string[];
