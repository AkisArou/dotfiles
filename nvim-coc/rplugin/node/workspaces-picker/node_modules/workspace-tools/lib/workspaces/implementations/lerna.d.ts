import { WorkspaceInfo } from "../../types/WorkspaceInfo";
/** Get package paths for a lerna workspace. */
export declare function getWorkspacePackagePaths(cwd: string): string[];
/**
 * Get an array with names, paths, and package.json contents for each package in a lerna workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getLernaWorkspaces(cwd: string): WorkspaceInfo;
/**
 * Get an array with names, paths, and package.json contents for each package in a lerna workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getLernaWorkspacesAsync(cwd: string): Promise<WorkspaceInfo>;
export { getLernaWorkspaces as getWorkspaces };
export { getLernaWorkspacesAsync as getWorkspacesAsync };
