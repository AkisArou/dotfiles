return {
  cmd = { vim.fn.fnamemodify("./node_modules/.bin/" .. "oxlint", ":p"), "--lsp" },
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
  on_attach = function(client, bufnr)
    vim.api.nvim_buf_create_user_command(bufnr, "LspOxlintFixAll", function()
      client:exec_cmd({
        title = "Apply Oxlint automatic fixes",
        command = "oxc.fixAll",
        arguments = { { uri = vim.uri_from_bufnr(bufnr) } },
      })
    end, {
      desc = "Apply Oxlint automatic fixes",
    })
  end,
  -- init_options = {
  --   settings = {
  --     -- ['run'] = 'onType',
  --     -- ['configPath'] = nil,
  --     -- ['tsConfigPath'] = nil,
  --     -- ['unusedDisableDirectives'] = 'allow',
  --     -- ['typeAware'] = false,
  --     -- ['disableNestedConfig'] = false,
  --     -- ['fixKind'] = 'safe_fix',
  --   },
  -- },
}
