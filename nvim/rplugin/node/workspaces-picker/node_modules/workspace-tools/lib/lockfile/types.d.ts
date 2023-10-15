export declare type Dependencies = {
    [key in string]: string;
};
export declare type LockDependency = {
    version: string;
    dependencies?: Dependencies;
};
export declare type ParsedLock = {
    type: "success" | "merge" | "conflict";
    object: {
        [key in string]: LockDependency;
    };
};
export interface PnpmLockFile {
    packages: {
        [name: string]: any;
    };
}
export interface NpmWorkspacesInfo {
    version: string;
    workspaces: {
        packages: string[];
    };
}
export interface NpmSymlinkInfo {
    resolved: string;
    link: boolean;
    integrity?: "sha512" | "sha1";
    dev?: boolean;
    optional?: boolean;
    devOptional?: boolean;
    dependencies?: {
        [key: string]: LockDependency;
    };
}
export interface NpmLockFile {
    name: string;
    version: string;
    lockfileVersion?: 1 | 2 | 3;
    requires?: boolean;
    packages?: {
        ""?: NpmWorkspacesInfo;
    } & {
        [key: string]: NpmSymlinkInfo | LockDependency;
    };
    dependencies?: {
        [key: string]: LockDependency;
    };
}
export interface BerryLockFile {
    __metadata: any;
    [key: string]: {
        version: string;
        dependencies: {
            [dependency: string]: string;
        };
    };
}
