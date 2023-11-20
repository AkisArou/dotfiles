"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.parseNpmLock = void 0;
const nameAtVersion_1 = require("./nameAtVersion");
function parseNpmLock(lock) {
    var _a;
    // Re-format the dependencies object so that the key includes the version, similarly to yarn.lock.
    // For example, `"@microsoft/task-scheduler": { }` will become `"@microsoft/task-scheduler@2.7.1": { }`.
    const dependencies = Object.fromEntries(Object.entries((_a = lock.dependencies) !== null && _a !== void 0 ? _a : {}).map(([key, dep]) => [(0, nameAtVersion_1.nameAtVersion)(key, dep.version), dep]));
    return {
        object: dependencies,
        type: "success",
    };
}
exports.parseNpmLock = parseNpmLock;
//# sourceMappingURL=parseNpmLock.js.map