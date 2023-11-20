"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.isCachingEnabled = exports.setCachingEnabled = void 0;
let cachingEnabled = true;
/** Enable or disable caching for all utilities that support caching */
function setCachingEnabled(enabled) {
    cachingEnabled = enabled;
}
exports.setCachingEnabled = setCachingEnabled;
function isCachingEnabled() {
    return cachingEnabled;
}
exports.isCachingEnabled = isCachingEnabled;
//# sourceMappingURL=isCachingEnabled.js.map