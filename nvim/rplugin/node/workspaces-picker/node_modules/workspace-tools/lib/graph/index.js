"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getDependentMap = void 0;
__exportStar(require("./createPackageGraph"), exports);
__exportStar(require("./createDependencyMap"), exports);
const createDependencyMap_1 = require("./createDependencyMap");
/**
 * @deprecated - use createDependencyMap() instead
 *
 * Gets a map that has the package name as key, and its dependencies as values
 */
function getDependentMap(packages) {
    return (0, createDependencyMap_1.createDependencyMap)(packages).dependencies;
}
exports.getDependentMap = getDependentMap;
//# sourceMappingURL=index.js.map