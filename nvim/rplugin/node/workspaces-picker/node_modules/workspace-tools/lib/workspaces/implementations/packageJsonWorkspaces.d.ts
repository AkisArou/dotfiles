export declare function getPackagePathsFromWorkspaceRoot(packageJsonWorkspacesRoot: string): string[];
/**
 * Get an array with names, paths, and package.json contents for each package in an npm/yarn workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getPackagePathsFromWorkspaceRootAsync(packageJsonWorkspacesRoot: string): Promise<string[]>;
/**
 * Get an array with names, paths, and package.json contents for each package in an npm/yarn workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getWorkspaceInfoFromWorkspaceRoot(packageJsonWorkspacesRoot: string): import("../..").WorkspaceInfo;
/**
 * Get an array with names, paths, and package.json contents for each package in an npm/yarn workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 */
export declare function getWorkspaceInfoFromWorkspaceRootAsync(packageJsonWorkspacesRoot: string): Promise<import("../..").WorkspaceInfo>;
