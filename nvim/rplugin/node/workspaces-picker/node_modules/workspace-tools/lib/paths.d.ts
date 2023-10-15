/**
 * Starting from `cwd`, searches up the directory hierarchy for `filePath`.
 * If multiple strings are given, searches each directory level for any of them.
 * @returns Full path to the item found, or undefined if not found.
 */
export declare function searchUp(filePath: string | string[], cwd: string): string | undefined;
/**
 * Starting from `cwd`, uses `git rev-parse --show-toplevel` to find the root of the git repo.
 * Throws if `cwd` is not in a Git repository.
 */
export declare function findGitRoot(cwd: string): string;
/**
 * Starting from `cwd`, searches up the directory hierarchy for `package.json`.
 */
export declare function findPackageRoot(cwd: string): string | undefined;
/**
 * Starting from `cwd`, searches up the directory hierarchy for the workspace root,
 * falling back to the git root if no workspace is detected.
 */
export declare function findProjectRoot(cwd: string): string;
export declare function isChildOf(child: string, parent: string): boolean;
