"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.createDependencyMap = void 0;
const getPackageDependencies_1 = require("./getPackageDependencies");
function createDependencyMap(packages, options = { withDevDependencies: true, withPeerDependencies: false }) {
    const map = {
        dependencies: new Map(),
        dependents: new Map(),
    };
    const internalPackages = new Set(Object.keys(packages));
    for (const [pkg, info] of Object.entries(packages)) {
        const deps = (0, getPackageDependencies_1.getPackageDependencies)(info, internalPackages, options);
        for (const dep of deps) {
            if (!map.dependencies.has(pkg)) {
                map.dependencies.set(pkg, new Set());
            }
            map.dependencies.get(pkg).add(dep);
            if (!map.dependents.has(dep)) {
                map.dependents.set(dep, new Set());
            }
            map.dependents.get(dep).add(pkg);
        }
    }
    return map;
}
exports.createDependencyMap = createDependencyMap;
//# sourceMappingURL=createDependencyMap.js.map