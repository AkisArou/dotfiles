"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspaceRoot = void 0;
const implementations_1 = require("./implementations");
/**
 * Get the root directory of a workspace/monorepo, defined as the directory where the workspace
 * manager config file is located.
 * @param cwd Start searching from here
 * @param preferredManager Search for only this manager's config file
 */
function getWorkspaceRoot(cwd, preferredManager) {
    var _a;
    return (_a = (0, implementations_1.getWorkspaceManagerAndRoot)(cwd, undefined, preferredManager)) === null || _a === void 0 ? void 0 : _a.root;
}
exports.getWorkspaceRoot = getWorkspaceRoot;
//# sourceMappingURL=getWorkspaceRoot.js.map