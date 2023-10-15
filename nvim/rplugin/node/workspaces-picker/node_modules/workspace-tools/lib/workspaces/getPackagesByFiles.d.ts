/**
 * Given a list of files, finds all packages names that contain those files
 *
 * @param workspaceRoot - The root of the workspace
 * @param files - files to search for
 * @param ignoreGlobs - glob patterns to ignore
 * @param returnAllPackagesOnNoMatch - if true, will return all packages if no matches are found
 * @returns package names that have changed
 */
export declare function getPackagesByFiles(workspaceRoot: string, files: string[], ignoreGlobs?: string[], returnAllPackagesOnNoMatch?: boolean): string[];
