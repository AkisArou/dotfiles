"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.logVerboseWarning = void 0;
/**
 * Helper that logs an error to `console.warn` if `process.env.VERBOSE` is set.
 * This should be replaced with a proper logging system eventually.
 */
function logVerboseWarning(description, err) {
    var _a;
    if (process.env.VERBOSE) {
        console.warn(`${description}${err ? ":\n" : ""}`, ((_a = err) === null || _a === void 0 ? void 0 : _a.stack) || err || "");
    }
}
exports.logVerboseWarning = logVerboseWarning;
//# sourceMappingURL=logging.js.map