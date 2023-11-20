/**
 * Given package folder globs (such as those from package.json `workspaces`) and a workspace root
 * directory, get paths to actual package folders.
 */
export declare function getPackagePaths(root: string, packageGlobs: string[]): string[];
/**
 * Given package folder globs (such as those from package.json `workspaces`) and a workspace root
 * directory, get paths to actual package folders.
 */
export declare function getPackagePathsAsync(root: string, packageGlobs: string[]): Promise<string[]>;
