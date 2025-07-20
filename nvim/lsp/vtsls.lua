local autoImportSpecifierExcludeRegex =
  "^(assert|async_hooks|buffer|child_process|cluster|console|crypto|dgram|dns|domain|events|fs|fs/promises|http|http2|https|inspector|module|net|os|path|path/posix|perf_hooks|process|punycode|querystring|readline|repl|stream|string_decoder|timers|tls|trace_events|tty|url|util|v8|vm|worker_threads|zlib)$"

local preferences = {
  includePackageJsonAutoImports = "on",
  importModuleSpecifier = "non-relative",
  importModuleSpecifierEnding = "js",
  autoImportSpecifierExcludeRegexes = {
    autoImportSpecifierExcludeRegex,
  },
}

--- @type vim.lsp.Config
return {
  root_dir = vim.fn.getcwd(),
  root_markers = { ".git", "tsconfig.json", "jsonconfig.json", "package.json" },
  settings = {
    vtsls = {
      autoUseWorkspaceTsdk = true,
      experimental = {
        completion = {
          enableServerSideFuzzyMatch = true,
          entriesLimit = 5000,
        },
      },
    },
    typescript = {
      format = {
        enable = false,
      },
      preferences = preferences,
    },
    javascript = {
      format = {
        enable = false,
      },
      preferences = preferences,
    },
  },
}
