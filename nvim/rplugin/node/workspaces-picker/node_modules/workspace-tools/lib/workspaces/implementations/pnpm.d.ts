import { WorkspaceInfo } from "../../types/WorkspaceInfo";
/** @deprecated Use `getWorkspaceRoot` */
export declare function getPnpmWorkspaceRoot(cwd: string): string;
/** Get package paths for a pnpm workspace. */
export declare function getWorkspacePackagePaths(cwd: string): string[];
/**
 * Get an array with names, paths, and package.json contents for each package in a pnpm workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getPnpmWorkspaces(cwd: string): WorkspaceInfo;
/**
 * Get an array with names, paths, and package.json contents for each package in a pnpm workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getPnpmWorkspacesAsync(cwd: string): Promise<WorkspaceInfo>;
export { getPnpmWorkspaces as getWorkspaces };
export { getPnpmWorkspacesAsync as getWorkspacesAsync };
