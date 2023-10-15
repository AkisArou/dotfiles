/**
 * Get a list of package folder paths in the workspace. The list of included packages is based on
 * the manager's config file and matching package folders (which must contain package.json) on disk.
 */
export declare function getWorkspacePackagePaths(cwd: string): string[];
/**
 * Get a list of package folder paths in the workspace. The list of included packages is based on
 * the manager's config file and matching package folders (which must contain package.json) on disk.
 */
export declare function getWorkspacePackagePathsAsync(cwd: string): Promise<string[]>;
