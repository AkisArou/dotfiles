import { PackageDependenciesOptions } from "./getPackageDependencies";
import { PackageInfos } from "../types/PackageInfo";
export interface DependencyMap {
    dependencies: Map<string, Set<string>>;
    dependents: Map<string, Set<string>>;
}
export declare function createDependencyMap(packages: PackageInfos, options?: PackageDependenciesOptions): DependencyMap;
