export * from "./createPackageGraph";
export * from "./createDependencyMap";
import { PackageInfos } from "../types/PackageInfo";
/**
 * @deprecated - use createDependencyMap() instead
 *
 * Gets a map that has the package name as key, and its dependencies as values
 */
export declare function getDependentMap(packages: PackageInfos): Map<string, Set<string>>;
