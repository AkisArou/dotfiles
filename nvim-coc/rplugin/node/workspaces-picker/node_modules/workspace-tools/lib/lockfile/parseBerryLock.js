"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.parseBerryLock = void 0;
function parseBerryLock(yaml) {
    var _a;
    const results = {};
    if (yaml) {
        for (const [keySpec, descriptor] of Object.entries(yaml)) {
            if (keySpec === "__metadata") {
                continue;
            }
            const keys = keySpec.split(", ");
            for (const key of keys) {
                const normalizedKey = normalizeKey(key);
                results[normalizedKey] = {
                    version: descriptor.version,
                    dependencies: (_a = descriptor.dependencies) !== null && _a !== void 0 ? _a : {},
                };
            }
        }
    }
    return {
        object: results,
        type: "success",
    };
}
exports.parseBerryLock = parseBerryLock;
// normalizes the version range as a key lookup
function normalizeKey(key) {
    if (key.includes("npm:")) {
        return key.replace(/npm:/, "");
    }
    return key;
}
//# sourceMappingURL=parseBerryLock.js.map