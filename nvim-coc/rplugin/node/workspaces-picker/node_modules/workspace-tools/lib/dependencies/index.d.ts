import { getTransitiveConsumers, getTransitiveProviders } from "./transitiveDeps";
import { getPackageDependencies } from "../graph/getPackageDependencies";
export declare const getTransitiveDependencies: typeof getTransitiveProviders;
export { getTransitiveProviders };
export declare const getTransitiveDependents: typeof getTransitiveConsumers;
export { getTransitiveConsumers };
/** @deprecated Do not use */
export declare const getInternalDeps: typeof getPackageDependencies;
