/**
 * Get a repository full name (owner and repo, plus organization for ADO/VSO) from a repository URL,
 * including special handling for the many ADO/VSO URL formats.
 *
 * Examples:
 * - returns `microsoft/workspace-tools` for `https://github.com/microsoft/workspace-tools.git`
 * - returns `foo/bar/some-repo` for `https://dev.azure.com/foo/bar/_git/some-repo`
 */
export declare function getRepositoryName(url: string): string;
