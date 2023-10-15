import { WorkspaceManager } from "../WorkspaceManager";
export interface WorkspaceManagerAndRoot {
    /** Workspace manager name */
    manager: WorkspaceManager;
    /** Workspace root, where the manager configuration file is located */
    root: string;
}
/**
 * Get the preferred workspace manager based on `process.env.PREFERRED_WORKSPACE_MANAGER`
 * (if valid).
 */
export declare function getPreferredWorkspaceManager(): WorkspaceManager | undefined;
/**
 * Get the workspace manager name and workspace root directory for `cwd`, with caching.
 * Also respects the `process.env.PREFERRED_WORKSPACE_MANAGER` override, provided the relevant
 * manager file exists.
 * @param cwd Directory to search up from
 * @param cache Optional override cache for testing
 * @param preferredManager Optional override manager (if provided, only searches for this manager's file)
 * @returns Workspace manager and root, or undefined if it can't be determined
 */
export declare function getWorkspaceManagerAndRoot(cwd: string, cache?: Map<string, WorkspaceManagerAndRoot | undefined>, preferredManager?: WorkspaceManager): WorkspaceManagerAndRoot | undefined;
