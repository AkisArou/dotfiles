import { WorkspaceInfo } from "../types/WorkspaceInfo";
/**
 * Get an array with names, paths, and package.json contents for each of the given package paths
 * within a workspace.
 *
 * This is an internal helper used by `getWorkspaces` implementations for different managers.
 * (See `../getWorkspaces` for why it's named this way.)
 * @param packagePaths Paths to packages within a workspace
 * @returns Array of workspace package infos
 * @internal
 */
export declare function getWorkspacePackageInfo(packagePaths: string[]): WorkspaceInfo;
/**
 * Get an array with names, paths, and package.json contents for each of the given package paths
 * within a workspace.
 *
 * This is an internal helper used by `getWorkspaces` implementations for different managers.
 * (See `../getWorkspaces` for why it's named this way.)
 * @param packagePaths Paths to packages within a workspace
 * @returns Array of workspace package infos
 * @internal
 */
export declare function getWorkspacePackageInfoAsync(packagePaths: string[]): Promise<WorkspaceInfo>;
