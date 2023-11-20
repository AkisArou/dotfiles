/**
 * Searches all package names based on "scoping" (i.e. "scope" in the sense of inclusion).
 * NOTE: this is not the same as package scopes (`@scope/package`).
 */
export declare function getScopedPackages(search: string[], packages: {
    [pkg: string]: unknown;
} | string[]): string[];
