export declare type GetDefaultRemoteOptions = {
    /** Get repository info relative to this directory. */
    cwd: string;
    /**
     * If true, throw an error if remote info can't be found, or if a `repository` is not specified
     * in package.json and no matching remote is found.
     */
    strict?: boolean;
    /** If true, log debug messages about how the remote was chosen */
    verbose?: boolean;
};
/**
 * Get the name of the default remote: the one matching the `repository` field in package.json.
 * Throws if `options.cwd` is not in a git repo or there's no package.json at the repo root.
 *
 * The order of preference for returned remotes is:
 * 1. If `repository` is defined in package.json, the remote with a matching URL (if `options.strict`
 *    is true, throws an error if no matching remote exists)
 * 2. `upstream` if defined
 * 3. `origin` if defined
 * 4. The first defined remote
 * 5. If there are no defined remotes: throws an error if `options.strict` is true; otherwise returns `origin`
 *
 * @returns The name of the inferred default remote.
 */
export declare function getDefaultRemote(options: GetDefaultRemoteOptions): string;
/** @deprecated Use the object param version */
export declare function getDefaultRemote(cwd: string): string;
