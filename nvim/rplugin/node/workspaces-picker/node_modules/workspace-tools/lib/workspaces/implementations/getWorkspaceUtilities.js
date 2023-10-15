"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getWorkspaceUtilities = void 0;
const getWorkspaceManagerAndRoot_1 = require("./getWorkspaceManagerAndRoot");
/**
 * Get utility implementations for the workspace manager of `cwd`.
 * Returns undefined if the manager can't be determined.
 */
function getWorkspaceUtilities(cwd) {
    var _a;
    const manager = (_a = (0, getWorkspaceManagerAndRoot_1.getWorkspaceManagerAndRoot)(cwd)) === null || _a === void 0 ? void 0 : _a.manager;
    switch (manager) {
        case "yarn":
            return require("./yarn");
        case "pnpm":
            return require("./pnpm");
        case "rush":
            return require("./rush");
        case "npm":
            return require("./npm");
        case "lerna":
            return require("./lerna");
    }
}
exports.getWorkspaceUtilities = getWorkspaceUtilities;
//# sourceMappingURL=getWorkspaceUtilities.js.map