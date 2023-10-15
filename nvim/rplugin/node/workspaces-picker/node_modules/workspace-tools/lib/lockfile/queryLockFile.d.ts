import { LockDependency, ParsedLock } from "./types";
export declare function queryLockFile(name: string, versionRange: string, lock: ParsedLock): LockDependency;
