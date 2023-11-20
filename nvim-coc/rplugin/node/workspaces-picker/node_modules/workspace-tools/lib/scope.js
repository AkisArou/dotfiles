"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getScopedPackages = void 0;
const micromatch_1 = __importDefault(require("micromatch"));
/**
 * Searches all package names based on "scoping" (i.e. "scope" in the sense of inclusion).
 * NOTE: this is not the same as package scopes (`@scope/package`).
 */
function getScopedPackages(search, packages) {
    const packageNames = Array.isArray(packages) ? packages : Object.keys(packages);
    const results = new Set();
    // perform a package-scoped search (e.g. search is @scope/foo*)
    const scopedSearch = search.filter((needle) => needle.startsWith("@") || needle.startsWith("!@"));
    if (scopedSearch.length > 0) {
        const matched = (0, micromatch_1.default)(packageNames, scopedSearch);
        for (const pkg of matched) {
            results.add(pkg);
        }
    }
    // perform a package-unscoped search (e.g. search is foo*)
    const unscopedSearch = search.filter((needle) => !needle.startsWith("@") && !needle.startsWith("!@"));
    if (unscopedSearch.length > 0) {
        // only generate the bare package map if there ARE unscoped searches
        const barePackageMap = generateBarePackageMap(packageNames);
        let matched = (0, micromatch_1.default)(Object.keys(barePackageMap), unscopedSearch);
        for (const bare of matched) {
            for (const pkg of barePackageMap[bare]) {
                results.add(pkg);
            }
        }
    }
    return [...results];
}
exports.getScopedPackages = getScopedPackages;
function generateBarePackageMap(packageNames) {
    const barePackageMap = {};
    // create a map of bare package name -> list of full package names
    // NOTE: do not perform barePackageMap lookup if any of the "scopes" arg starts with "@"
    for (const pkg of packageNames) {
        const bare = pkg.replace(/^@[^/]+\//, "");
        barePackageMap[bare] = barePackageMap[bare] || [];
        barePackageMap[bare].push(pkg);
    }
    return barePackageMap;
}
//# sourceMappingURL=scope.js.map