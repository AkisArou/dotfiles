import { WorkspaceManager } from "./WorkspaceManager";
/**
 * Get the root directory of a workspace/monorepo, defined as the directory where the workspace
 * manager config file is located.
 * @param cwd Start searching from here
 * @param preferredManager Search for only this manager's config file
 */
export declare function getWorkspaceRoot(cwd: string, preferredManager?: WorkspaceManager): string | undefined;
