import { WorkspaceInfo } from "../../types/WorkspaceInfo";
export interface WorkspaceUtilities {
    /**
     * Get an array of paths to packages in the workspace, based on the manager's config file.
     */
    getWorkspacePackagePaths: (cwd: string) => string[];
    /**
     * Get an array of paths to packages in the workspace, based on the manager's config file.
     */
    getWorkspacePackagePathsAsync?: (cwd: string) => Promise<string[]>;
    /**
     * Get an array with names, paths, and package.json contents for each package in a workspace.
     * (See `../getWorkspaces` for why it's named this way.)
     */
    getWorkspaces: (cwd: string) => WorkspaceInfo;
    /**
     * Get an array with names, paths, and package.json contents for each package in a workspace.
     * (See `../getWorkspaces` for why it's named this way.)
     */
    getWorkspacesAsync?: (cwd: string) => Promise<WorkspaceInfo>;
}
/**
 * Get utility implementations for the workspace manager of `cwd`.
 * Returns undefined if the manager can't be determined.
 */
export declare function getWorkspaceUtilities(cwd: string): WorkspaceUtilities | undefined;
