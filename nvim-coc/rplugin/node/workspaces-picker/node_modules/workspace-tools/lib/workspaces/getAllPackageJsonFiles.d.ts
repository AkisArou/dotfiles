/**
 * Get paths to every package.json in the workspace, given a cwd.
 */
export declare function getAllPackageJsonFiles(cwd: string): string[];
export declare function _resetPackageJsonFilesCache(): void;
/**
 * Get paths to every package.json in the workspace, given a cwd.
 */
export declare function getAllPackageJsonFilesAsync(cwd: string): Promise<string[]>;
