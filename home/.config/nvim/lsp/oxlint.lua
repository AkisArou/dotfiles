local function local_oxlint(config)
  local cmd = config.root_dir and config.root_dir .. "/node_modules/.bin/oxlint"
  if cmd and vim.fn.executable(cmd) == 1 then
    return cmd
  end
end

return {
  cmd = function(dispatchers, config)
    local cmd = local_oxlint(config)
    local env
    if not cmd and vim.fn.executable("npm") == 1 then
      -- The fallback config can reference JS plugins installed globally with npm.
      local npm_root = vim.fn.systemlist({ "npm", "root", "-g" })[1]
      if vim.v.shell_error == 0 and npm_root and npm_root ~= "" then
        env = { NODE_PATH = npm_root .. (vim.env.NODE_PATH and ":" .. vim.env.NODE_PATH or "") }
      end
    end
    return vim.lsp.rpc.start({ cmd or "oxlint", "--lsp" }, dispatchers, { env = env })
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
  workspace_required = true,
  on_attach = function(_, bufnr)
    vim.api.nvim_buf_create_user_command(bufnr, "LspOxlintFixAll", function()
      require("custom.oxlint").fix_all(bufnr)
    end, {
      desc = "Apply Oxlint automatic fixes",
    })
  end,
  before_init = function(params, config)
    local settings = config.settings or {}
    if not local_oxlint(config) then
      settings = vim.tbl_extend("force", settings, {
        configPath = vim.fn.expand("~/dotfiles/home/.config/nvim/.oxlintrc.json"),
      })
    end
    config.settings = settings
    params.initializationOptions = {
      { workspaceUri = vim.uri.from_fname(config.root_dir), options = settings },
    }
  end,
}
