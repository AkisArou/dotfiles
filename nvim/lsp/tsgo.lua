local preferences = {
  includePackageJsonAutoImports = "on",
  importModuleSpecifier = "non-relative",
  importModuleSpecifierEnding = "js",
  useAliasesForRenames = false,
  autoImportSpecifierExcludeRegexes = {
    "^(assert|async_hooks|buffer|child_process|cluster|console|crypto|dgram|dns|domain|events|fs|fs/promises|http|http2|https|inspector|module|net|os|path|path/posix|perf_hooks|process|punycode|querystring|readline|repl|stream|string_decoder|timers|tls|trace_events|tty|url|util|v8|vm|worker_threads|zlib)$",
  },
}

---@type vim.lsp.Config
return {
  cmd = function(dispatchers, config)
    local cmd = "tsgo"
    local local_cmd = (config or {}).root_dir and config.root_dir .. "/node_modules/.bin/tsgo"
    if local_cmd and vim.fn.executable(local_cmd) == 1 then
      cmd = local_cmd
    end
    return vim.lsp.rpc.start({ cmd, "--lsp", "--stdio" }, dispatchers)
  end,
  filetypes = {
    "javascript",
    "javascriptreact",
    "javascript.jsx",
    "typescript",
    "typescriptreact",
    "typescript.tsx",
  },
  root_dir = vim.fn.getcwd(),
  settings = {
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
