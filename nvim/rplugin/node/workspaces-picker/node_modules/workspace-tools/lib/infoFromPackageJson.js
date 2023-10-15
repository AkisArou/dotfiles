"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.infoFromPackageJson = void 0;
function infoFromPackageJson(packageJson, packageJsonPath) {
    return {
        packageJsonPath,
        ...packageJson,
    };
}
exports.infoFromPackageJson = infoFromPackageJson;
//# sourceMappingURL=infoFromPackageJson.js.map