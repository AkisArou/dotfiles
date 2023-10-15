import { PackageInfos } from "../types/PackageInfo";
export declare function getDependentMap(packages: PackageInfos): Map<string, Set<string>>;
/**
 * For a package graph of `a->b->c` (where `b` depends on `a`), transitive consumers of `a` are `b` & `c`
 * and their consumers (or what are the consequences of `a`)
 * @deprecated Do not use
 */
export declare function getTransitiveConsumers(targets: string[], packages: PackageInfos, scope?: string[]): string[];
/**
 * For a package graph of `a->b->c` (where `b` depends on `a`), transitive providers of `c` are `a` & `b`
 * and their providers (or what is needed to satisfy `c`)
 *
 * @deprecated Do not use
 */
export declare function getTransitiveProviders(targets: string[], packages: PackageInfos): string[];
