import { PackageInfos } from "./types/PackageInfo";
export declare function getPackageInfos(cwd: string): PackageInfos;
export declare function getPackageInfosAsync(cwd: string): Promise<PackageInfos>;
