"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.findWorkspacePath = void 0;
/**
 * Find the path for a particular package name from an array of info about packages within a workspace.
 * (See `../getWorkspaces` for why it's named this way.)
 * @param workspaces Array of info about packages within a workspace
 * @param packageName Package name to find
 * @returns Package path if found, or undefined
 */
function findWorkspacePath(workspaces, packageName) {
    var _a;
    return (_a = workspaces.find(({ name }) => name === packageName)) === null || _a === void 0 ? void 0 : _a.path;
}
exports.findWorkspacePath = findWorkspacePath;
//# sourceMappingURL=findWorkspacePath.js.map