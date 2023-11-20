import { PackageInfo } from "../types/PackageInfo";
export interface PackageDependenciesOptions {
    withDevDependencies?: boolean;
    withPeerDependencies?: boolean;
}
export declare function getPackageDependencies(info: PackageInfo, packages: Set<string>, options?: PackageDependenciesOptions): string[];
