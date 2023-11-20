import { GetDefaultRemoteOptions } from "./getDefaultRemote";
export declare type GetDefaultRemoteBranchOptions = GetDefaultRemoteOptions & {
    /** Name of branch to use. If undefined, uses the default branch name (falling back to `master`). */
    branch?: string;
};
/**
 * Gets a reference to `options.branch` or the default branch relative to the default remote.
 * (See {@link getDefaultRemote} for how the default remote is determined.)
 * Throws if `options.cwd` is not in a git repo or there's no package.json at the repo root.
 * @returns A branch reference like `upstream/master` or `origin/master`.
 */
export declare function getDefaultRemoteBranch(options: GetDefaultRemoteBranchOptions): string;
/**
 * First param: `branch`. Second param: `cwd`. See {@link GetDefaultRemoteBranchOptions} for more info.
 * (This had to be changed to `...args` to avoid a conflict with the object param version.)
 * @deprecated Use the object param version
 */
export declare function getDefaultRemoteBranch(...args: string[]): string;
