/// <reference types="node" />
import { SpawnSyncReturns } from "child_process";
import { SpawnSyncOptions } from "child_process";
export declare class GitError extends Error {
    originalError: unknown;
    constructor(message: string, originalError?: unknown);
}
export declare type GitProcessOutput = {
    stderr: string;
    stdout: string;
    success: boolean;
} & Omit<SpawnSyncReturns<string | Buffer>, "stdout" | "stderr">;
/** Observes the git operations called from `git()` or `gitFailFast()` */
export declare type GitObserver = (args: string[], output: GitProcessOutput) => void;
/**
 * Adds an observer for the git operations, e.g. for testing
 * @returns a function to remove the observer
 */
export declare function addGitObserver(observer: GitObserver): () => void;
/** Clear all git observers */
export declare function clearGitObservers(): void;
/**
 * Runs git command - use this for read-only commands
 */
export declare function git(args: string[], options?: SpawnSyncOptions): GitProcessOutput;
/**
 * Runs git command - use this for commands that make changes to the filesystem
 */
export declare function gitFailFast(args: string[], options?: SpawnSyncOptions & {
    noExitCode?: boolean;
}): void;
