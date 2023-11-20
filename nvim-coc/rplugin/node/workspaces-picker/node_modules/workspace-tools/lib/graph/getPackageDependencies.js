"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getPackageDependencies = void 0;
function getPackageDependencies(info, packages, options = { withDevDependencies: true }) {
    const deps = [];
    if (info.dependencies) {
        for (const dep of Object.keys(info.dependencies)) {
            if (dep !== info.name && packages.has(dep)) {
                deps.push(dep);
            }
        }
    }
    if (info.devDependencies && options.withDevDependencies) {
        for (const dep of Object.keys(info.devDependencies)) {
            if (dep !== info.name && packages.has(dep)) {
                deps.push(dep);
            }
        }
    }
    if (info.peerDependencies && options.withPeerDependencies) {
        for (const dep of Object.keys(info.peerDependencies)) {
            if (dep !== info.name && packages.has(dep)) {
                deps.push(dep);
            }
        }
    }
    return deps;
}
exports.getPackageDependencies = getPackageDependencies;
//# sourceMappingURL=getPackageDependencies.js.map