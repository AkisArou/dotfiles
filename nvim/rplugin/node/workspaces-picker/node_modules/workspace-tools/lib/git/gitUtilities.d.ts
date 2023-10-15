export declare function getUntrackedChanges(cwd: string): string[];
export declare function fetchRemote(remote: string, cwd: string): void;
export declare function fetchRemoteBranch(remote: string, remoteBranch: string, cwd: string): void;
/**
 * Gets all the changes that have not been staged yet
 * @param cwd
 */
export declare function getUnstagedChanges(cwd: string): string[];
export declare function getChanges(branch: string, cwd: string): string[];
/**
 * Gets all the changes between the branch and the merge-base
 */
export declare function getBranchChanges(branch: string, cwd: string): string[];
export declare function getChangesBetweenRefs(fromRef: string, toRef: string, options: string[], pattern: string, cwd: string): string[];
export declare function getStagedChanges(cwd: string): string[];
export declare function getRecentCommitMessages(branch: string, cwd: string): string[];
export declare function getUserEmail(cwd: string): string | null;
export declare function getBranchName(cwd: string): string | null;
export declare function getFullBranchRef(branch: string, cwd: string): string | null;
export declare function getShortBranchName(fullBranchRef: string, cwd: string): string | null;
export declare function getCurrentHash(cwd: string): string | null;
/**
 * Get the commit hash in which the file was first added.
 */
export declare function getFileAddedHash(filename: string, cwd: string): string | undefined;
export declare function init(cwd: string, email?: string, username?: string): void;
export declare function stage(patterns: string[], cwd: string): void;
export declare function commit(message: string, cwd: string, options?: string[]): void;
export declare function stageAndCommit(patterns: string[], message: string, cwd: string, commitOptions?: string[]): void;
export declare function revertLocalChanges(cwd: string): boolean;
export declare function getParentBranch(cwd: string): string | null;
export declare function getRemoteBranch(branch: string, cwd: string): string | null;
export declare function parseRemoteBranch(branch: string): {
    remote: string;
    remoteBranch: string;
};
/**
 * Gets the default branch based on `git config init.defaultBranch`, falling back to `master`.
 */
export declare function getDefaultBranch(cwd: string): string;
export declare function listAllTrackedFiles(patterns: string[], cwd: string): string[];
