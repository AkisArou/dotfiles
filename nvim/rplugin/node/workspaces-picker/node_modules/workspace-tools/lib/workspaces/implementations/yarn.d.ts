import { WorkspaceInfo } from "../../types/WorkspaceInfo";
/** @deprecated Use `getWorkspaceRoot` */
export declare function getYarnWorkspaceRoot(cwd: string): string;
/** Get package paths for a yarn workspace. */
export declare function getWorkspacePackagePaths(cwd: string): string[];
/** Get package paths for a yarn workspace. */
export declare function getWorkspacePackagePathsAsync(cwd: string): Promise<string[]>;
/**
 * Get an array with names, paths, and package.json contents for each package in a yarn workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getYarnWorkspaces(cwd: string): WorkspaceInfo;
/**
 * Get an array with names, paths, and package.json contents for each package in a yarn workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getYarnWorkspacesAsync(cwd: string): Promise<WorkspaceInfo>;
export { getYarnWorkspaces as getWorkspaces };
export { getYarnWorkspacesAsync as getWorkspacesAsync };
