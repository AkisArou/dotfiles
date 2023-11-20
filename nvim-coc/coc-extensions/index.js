"use strict";
var __create = Object.create;
var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __getProtoOf = Object.getPrototypeOf;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toESM = (mod, isNodeMode, target) => (target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
  // If the importer is in node compatibility mode or this is not an ESM
  // file that has been converted to a CommonJS file using a Babel-
  // compatible transform (i.e. "__esModule" has not been set), then set
  // "default" to the CommonJS "module.exports" for node compatibility.
  isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target,
  mod
));

// src/index.ts
var import_coc = require("coc.nvim");
var import_path = __toESM(require("path"));
var import_promises = require("fs/promises");
var import_process = require("process");
var import_fs = require("fs");
var TSCONFIG_BASE = "tsconfig.base.json";
var TSCONFIG_LIB = "tsconfig.lib.json";
var typeScriptExtensionId = "coc-tsserver";
function logCocInfo(...vals) {
  console.log(`NX-Coc: `, ...vals);
}
var tagMappings;
async function makeScopeTagMappings(workspaceRoot) {
  const tsconfigBase = (await readAndCacheJsonFile(TSCONFIG_BASE, workspaceRoot)).json;
  const paths = tsconfigBase.compilerOptions.paths;
  if (!paths) {
    logCocInfo("no paths");
    return [];
  }
  const libPaths = Object.entries(paths).map(([key, indexPath]) => {
    return {
      libName: key,
      indexPath: indexPath[0]
    };
  });
  const mappings = libPaths.map((libPath) => {
    try {
      const projectJsonPath = libPath.indexPath.replace("src/index.ts", "project.json");
      const projectJson = JSON.parse((0, import_fs.readFileSync)((0, import_path.join)(workspaceRoot, projectJsonPath), "utf-8"));
      if (!projectJson.tags) {
        return {
          path: libPath,
          scopeTag: void 0
        };
      }
      return {
        path: libPath,
        scopeTag: projectJson.tags.find((t) => t.includes("scope:"))
      };
    } catch (error) {
      logCocInfo(error);
      return {
        path: libPath,
        scopeTag: void 0
      };
    }
  });
  logCocInfo(mappings);
  tagMappings = mappings;
}
async function activate(context) {
  const tsExtension = import_coc.extensions.all.find((e) => e.id === typeScriptExtensionId);
  const workspaceRoot = (0, import_process.cwd)();
  logCocInfo("tsExtension", tsExtension);
  if (!tsExtension) {
    logCocInfo("no tsExtension found");
    return;
  }
  await tsExtension.activate();
  if (!tsExtension.exports) {
    logCocInfo("no tsExtension.exports found");
    return;
  }
  const api = tsExtension.exports;
  logCocInfo("api ", api);
  if (!api) {
    logCocInfo("no api found");
    return;
  }
  import_coc.workspace.onDidOpenTextDocument(
    (document) => {
      if (document.uri.endsWith(".ts") || document.uri.endsWith(".tsx")) {
        configurePlugin(workspaceRoot, api, document.uri);
      }
    },
    void 0,
    context.subscriptions
  );
  watchFile(
    `${workspaceRoot}/${TSCONFIG_BASE}`,
    () => {
      clearJsonCache(TSCONFIG_BASE, workspaceRoot);
      configurePlugin(workspaceRoot, api);
      makeScopeTagMappings(workspaceRoot);
    },
    context.subscriptions
  );
  import_coc.workspace.onDidChangeTextDocument(
    ({ textDocument }) => {
      if (textDocument.uri.endsWith(TSCONFIG_BASE)) {
        configurePlugin(workspaceRoot, api);
      }
    },
    void 0,
    context.subscriptions
  );
  makeScopeTagMappings(workspaceRoot);
}
function makePath(p) {
  return p.split("nable-solutions/")[1].split("src")[0] + "src/index.ts";
}
async function configurePlugin(workspaceRoot, api, documentUri) {
  const enableLibraryImports = true;
  if (enableLibraryImports) {
    let externalFiles = await getExternalFiles(workspaceRoot);
    if (documentUri && documentUri.includes("nable-solutions/packages")) {
      const indexPath = makePath(documentUri);
      const found = tagMappings.find((tm) => {
        return tm.path.indexPath === indexPath;
      });
      if (found) {
        externalFiles = externalFiles.filter((ef) => {
          var _a;
          const foundScopeTag = (_a = tagMappings.find((tm) => makePath(ef.mainFile) === tm.path.indexPath)) == null ? void 0 : _a.scopeTag;
          if (!foundScopeTag) {
            return true;
          }
          return found.scopeTag === foundScopeTag;
        });
      }
    }
    api.configurePlugin("@monodon/typescript-nx-imports-plugin", {
      externalFiles
    });
  }
}
async function getExternalFiles(workspaceRoot) {
  let tsconfig = (await readAndCacheJsonFile(TSCONFIG_BASE, workspaceRoot)).json;
  if (!("compilerOptions" in tsconfig)) {
    tsconfig = (await readAndCacheJsonFile("tsconfig.json", workspaceRoot)).json;
    if (!("compilerOptions" in tsconfig)) {
      return [];
    }
  }
  const paths = tsconfig.compilerOptions.paths || {};
  const externals = [];
  for (const [, value] of Object.entries(paths)) {
    const mainFile = (0, import_path.join)(workspaceRoot, value[0]);
    const configFilePath = await findConfig(mainFile, TSCONFIG_LIB);
    if (!configFilePath) {
      continue;
    }
    const directory = (0, import_path.dirname)(configFilePath);
    externals.push({ mainFile, directory });
  }
  return externals;
}
function watchFile(filePath, callback, disposable) {
  const filewatcher = import_coc.workspace.createFileSystemWatcher(filePath);
  filewatcher.onDidChange(callback, disposable);
  return filewatcher;
}
async function forEachAncestorDirectory(directory, callback) {
  while (true) {
    const result = await callback(directory);
    if (result !== void 0) {
      return result;
    }
    const parentPath = (0, import_path.dirname)(directory);
    if (parentPath === directory) {
      return void 0;
    }
    directory = parentPath;
  }
}
async function findConfig(searchPath, configName) {
  return forEachAncestorDirectory(searchPath, async (ancestor) => {
    const fileName = (0, import_path.join)(ancestor, configName);
    try {
      if (await fileExists(fileName)) {
        return fileName;
      }
    } catch (e) {
      return void 0;
    }
  });
}
async function fileExists(filePath) {
  try {
    return (await (0, import_promises.stat)(filePath)).isFile();
  } catch {
    return false;
  }
}
var fileContents = {};
async function readAndParseJson(filePath) {
  const content = await (0, import_promises.readFile)(filePath, { encoding: "utf-8" });
  try {
    return JSON.parse(content);
  } catch {
    throw new Error("CHECK THIS AKIS");
    const errors = [];
    if (errors.length > 0) {
      for (const { error, offset } of errors) {
      }
    }
  }
}
function clearJsonCache(filePath, basedir = "") {
  const fullFilePath = import_path.default.join(basedir, filePath);
  return delete fileContents[fullFilePath];
}
async function readAndCacheJsonFile(filePath, basedir = "") {
  if (!filePath) {
    logCocInfo("readAndCacheJsonFile no filePath", filePath);
    return {
      path: "",
      json: {}
    };
  }
  let fullFilePath = import_path.default.join(basedir, filePath);
  if (fullFilePath.startsWith("file:\\")) {
    fullFilePath = fullFilePath.replace("file:\\", "");
  }
  try {
    const stats = await (0, import_promises.stat)(fullFilePath);
    if (fileContents[fullFilePath] || stats.isFile()) {
      fileContents[fullFilePath] ||= await readAndParseJson(fullFilePath);
      return {
        path: fullFilePath,
        json: fileContents[fullFilePath]
      };
    }
  } catch (e) {
    logCocInfo("readAndCacheJsonFile error");
  }
  return {
    path: fullFilePath,
    json: {}
  };
}
exports.activate = activate;
