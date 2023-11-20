import { WorkspaceInfo } from "../../types/WorkspaceInfo";
/** @deprecated Use getWorkspaceRoot */
export declare function getRushWorkspaceRoot(cwd: string): string;
/** Get package paths for a rush workspace. */
export declare function getWorkspacePackagePaths(cwd: string): string[];
/**
 * Get an array with names, paths, and package.json contents for each package in a rush workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getRushWorkspaces(cwd: string): WorkspaceInfo;
/**
 * Get an array with names, paths, and package.json contents for each package in a rush workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getRushWorkspacesAsync(cwd: string): Promise<WorkspaceInfo>;
export { getRushWorkspaces as getWorkspaces };
export { getRushWorkspacesAsync as getWorkspacesAsync };
