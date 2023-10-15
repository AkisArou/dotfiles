"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getInternalDeps = exports.getTransitiveConsumers = exports.getTransitiveDependents = exports.getTransitiveProviders = exports.getTransitiveDependencies = void 0;
const transitiveDeps_1 = require("./transitiveDeps");
Object.defineProperty(exports, "getTransitiveConsumers", { enumerable: true, get: function () { return transitiveDeps_1.getTransitiveConsumers; } });
Object.defineProperty(exports, "getTransitiveProviders", { enumerable: true, get: function () { return transitiveDeps_1.getTransitiveProviders; } });
const getPackageDependencies_1 = require("../graph/getPackageDependencies");
// Some deprecated functions below for backwards compatibility
exports.getTransitiveDependencies = transitiveDeps_1.getTransitiveProviders;
exports.getTransitiveDependents = transitiveDeps_1.getTransitiveConsumers;
/** @deprecated Do not use */
exports.getInternalDeps = getPackageDependencies_1.getPackageDependencies;
//# sourceMappingURL=index.js.map