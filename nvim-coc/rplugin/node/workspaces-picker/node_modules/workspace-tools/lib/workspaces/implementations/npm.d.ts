import { WorkspaceInfo } from "../../types/WorkspaceInfo";
/** Get package paths for an npm workspace. */
export declare function getWorkspacePackagePaths(cwd: string): string[];
/** Get package paths for an npm workspace. */
export declare function getWorkspacePackagePathsAsync(cwd: string): Promise<string[]>;
/**
 * Get an array with names, paths, and package.json contents for each package in an npm workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getNpmWorkspaces(cwd: string): WorkspaceInfo;
/**
 * Get an array with names, paths, and package.json contents for each package in an npm workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getNpmWorkspacesAsync(cwd: string): Promise<WorkspaceInfo>;
export { getNpmWorkspaces as getWorkspaces };
export { getNpmWorkspacesAsync as getWorkspacesAsync };
