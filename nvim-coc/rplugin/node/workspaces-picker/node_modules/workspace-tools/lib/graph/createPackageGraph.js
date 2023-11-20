"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.createPackageGraph = void 0;
const createDependencyMap_1 = require("./createDependencyMap");
const micromatch_1 = __importDefault(require("micromatch"));
function createPackageGraph(packages, filters) {
    /** set of packages being accumulated as the graph is filtered */
    const packageSet = new Set();
    /** array of package names & its dependency being accumulated as the graph is filtered */
    const edges = [];
    const edgeKeys = new Set();
    let dependencyMapWithPeerDevDeps = undefined;
    let dependencyMapWithPeerDeps = undefined;
    let dependencyMapWithDevDeps = undefined;
    let dependencyMapWithoutPeerDevDeps = undefined;
    function visitorForFilter(filter, pkg, dependencies, dependents) {
        packageSet.add(pkg);
        if (!filter || (filter.includeDependencies && dependencies)) {
            for (const dep of dependencies) {
                const key = edgeKey(pkg, dep);
                if (!edgeKeys.has(key)) {
                    edgeKeys.add(key);
                    edges.push({ name: pkg, dependency: dep });
                }
                packageSet.add(dep);
            }
        }
        if (!filter || (filter.includeDependents && dependents)) {
            for (const dep of dependents) {
                const key = edgeKey(dep, pkg);
                if (!edgeKeys.has(key)) {
                    edgeKeys.add(key);
                    edges.push({ name: dep, dependency: pkg });
                }
                packageSet.add(dep);
            }
        }
    }
    if (filters) {
        filters = Array.isArray(filters) ? filters : [filters];
        for (const filter of filters) {
            const visitor = visitorForFilter.bind(undefined, filter);
            const dependencyMap = getDependencyMapForFilter(packages, filter);
            visitPackageGraph(packages, dependencyMap, visitor, filter);
        }
    }
    else {
        const visitor = visitorForFilter.bind(undefined, undefined);
        const dependencyMap = getDependencyMapForFilter(packages);
        visitPackageGraph(packages, dependencyMap, visitor);
    }
    return { packages: [...packageSet], dependencies: edges };
    /** calculates a key, for looking up whether an edge is already added */
    function edgeKey(name, dependency) {
        return `${name}->${dependency}`;
    }
    /** gets the dependencyMap for a filter - with or without devDeps */
    function getDependencyMapForFilter(packages, filter) {
        if (!filter) {
            return (0, createDependencyMap_1.createDependencyMap)(packages);
        }
        if (filter.withDevDependencies && filter.withPeerDependencies) {
            dependencyMapWithPeerDevDeps !== null && dependencyMapWithPeerDevDeps !== void 0 ? dependencyMapWithPeerDevDeps : (dependencyMapWithPeerDevDeps = (0, createDependencyMap_1.createDependencyMap)(packages, filter));
            return dependencyMapWithPeerDevDeps;
        }
        if (filter.withDevDependencies && !filter.withPeerDependencies) {
            dependencyMapWithDevDeps !== null && dependencyMapWithDevDeps !== void 0 ? dependencyMapWithDevDeps : (dependencyMapWithDevDeps = (0, createDependencyMap_1.createDependencyMap)(packages, filter));
            return dependencyMapWithDevDeps;
        }
        if (!filter.withDevDependencies && filter.withPeerDependencies) {
            dependencyMapWithPeerDeps !== null && dependencyMapWithPeerDeps !== void 0 ? dependencyMapWithPeerDeps : (dependencyMapWithPeerDeps = (0, createDependencyMap_1.createDependencyMap)(packages, filter));
            return dependencyMapWithPeerDeps;
        }
        dependencyMapWithoutPeerDevDeps !== null && dependencyMapWithoutPeerDevDeps !== void 0 ? dependencyMapWithoutPeerDevDeps : (dependencyMapWithoutPeerDevDeps = (0, createDependencyMap_1.createDependencyMap)(packages, filter));
        return dependencyMapWithoutPeerDevDeps;
    }
}
exports.createPackageGraph = createPackageGraph;
function visitPackageGraph(packages, dependencyMap, visitor, filter) {
    var _a, _b;
    const visited = new Set();
    const packageNames = Object.keys(packages);
    const stack = filter ? (0, micromatch_1.default)(packageNames, filter.namePatterns) : packageNames;
    while (stack.length > 0) {
        const pkg = stack.pop();
        if (visited.has(pkg)) {
            continue;
        }
        const nextPkgs = new Set();
        let dependencies = [];
        let dependents = [];
        if (!filter || filter.includeDependencies) {
            dependencies = [...((_a = dependencyMap.dependencies.get(pkg)) !== null && _a !== void 0 ? _a : [])];
            for (const dep of dependencies) {
                nextPkgs.add(dep);
            }
        }
        if (!filter || filter.includeDependents) {
            dependents = [...((_b = dependencyMap.dependents.get(pkg)) !== null && _b !== void 0 ? _b : [])];
            for (const dep of dependents) {
                nextPkgs.add(dep);
            }
        }
        visitor(pkg, dependencies, dependents);
        visited.add(pkg);
        if (nextPkgs.size > 0) {
            for (const nextPkg of nextPkgs) {
                stack.push(nextPkg);
            }
        }
    }
}
//# sourceMappingURL=createPackageGraph.js.map