import { ParsedLock } from "./types";
import { nameAtVersion } from "./nameAtVersion";
export declare function parseLockFile(packageRoot: string): Promise<ParsedLock>;
export { nameAtVersion };
export { queryLockFile } from "./queryLockFile";
export * from "./types";
