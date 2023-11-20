"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.queryLockFile = void 0;
const nameAtVersion_1 = require("./nameAtVersion");
function queryLockFile(name, versionRange, lock) {
    const versionRangeSignature = (0, nameAtVersion_1.nameAtVersion)(name, versionRange);
    return lock.object[versionRangeSignature];
}
exports.queryLockFile = queryLockFile;
//# sourceMappingURL=queryLockFile.js.map