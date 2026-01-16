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
}
