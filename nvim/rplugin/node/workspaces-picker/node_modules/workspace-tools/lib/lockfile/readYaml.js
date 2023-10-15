"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.readYaml = void 0;
const fs_1 = __importDefault(require("fs"));
function readYaml(file) {
    // This is delay loaded to avoid the perf penalty of parsing YAML utilities any time the package
    // is used (since usage of the YAML utilities is less common).
    const jsYaml = require("js-yaml");
    const content = fs_1.default.readFileSync(file, "utf8");
    return jsYaml.load(content);
}
exports.readYaml = readYaml;
//# sourceMappingURL=readYaml.js.map