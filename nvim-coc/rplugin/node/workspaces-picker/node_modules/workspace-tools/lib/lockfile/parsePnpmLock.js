"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.parsePnpmLock = void 0;
const nameAtVersion_1 = require("./nameAtVersion");
function parsePnpmLock(yaml) {
    const object = {};
    if (yaml && yaml.packages) {
        for (const [pkgSpec, snapshot] of Object.entries(yaml.packages)) {
            // TODO: handle file:foo.tgz syntax (rush uses this for internal package links)
            const specParts = pkgSpec.split(/\//);
            const name = specParts.length > 3 ? `${specParts[1]}/${specParts[2]}` : specParts[1];
            const version = specParts.length > 3 ? specParts[3] : specParts[2];
            object[(0, nameAtVersion_1.nameAtVersion)(name, version)] = {
                version,
                dependencies: snapshot.dependencies,
            };
        }
    }
    return {
        object,
        type: "success",
    };
}
exports.parsePnpmLock = parsePnpmLock;
//# sourceMappingURL=parsePnpmLock.js.map