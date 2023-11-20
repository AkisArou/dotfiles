import type { PackageInfos } from "../types/PackageInfo";
import type { PackageGraph } from "../types/PackageGraph";
export interface PackageGraphFilter {
    namePatterns: string[];
    includeDependencies?: boolean;
    includeDependents?: boolean;
    withDevDependencies?: boolean;
    withPeerDependencies?: boolean;
}
export declare function createPackageGraph(packages: PackageInfos, filters?: PackageGraphFilter[] | PackageGraphFilter): PackageGraph;
