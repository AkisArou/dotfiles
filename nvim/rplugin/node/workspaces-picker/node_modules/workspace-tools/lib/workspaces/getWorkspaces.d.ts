import { WorkspaceInfo } from "../types/WorkspaceInfo";
/**
 * Get an array with names, paths, and package.json contents for each package in a workspace.
 * The list of included packages is based on the workspace manager's config file.
 *
 * The method name is somewhat misleading due to the double meaning of "workspace", but it's retained
 * for compatibility. "Workspace" here refers to an individual package, in the sense of the `workspaces`
 * package.json config used by npm/yarn (instead of referring to the entire monorepo).
 */
export declare function getWorkspaces(cwd: string): WorkspaceInfo;
/**
 * Get an array with names, paths, and package.json contents for each package in a workspace.
 * The list of included packages is based on the workspace manager's config file.
 *
 * The method name is somewhat misleading due to the double meaning of "workspace", but it's retained
 * for compatibility. "Workspace" here refers to an individual package, in the sense of the `workspaces`
 * package.json config used by npm/yarn (instead of referring to the entire monorepo).
 */
export declare function getWorkspacesAsync(cwd: string): Promise<WorkspaceInfo>;
