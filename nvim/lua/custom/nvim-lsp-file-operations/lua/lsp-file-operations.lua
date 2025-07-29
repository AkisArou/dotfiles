local M = {}

M.setup = function()
  vim.lsp.config("*", {
    capabilities = {
      workspace = {
        fileOperations = {
          willRenameFiles = true,
          didRenameFiles = true,
          willCreateFiles = true,
          didCreateFiles = true,
          willDeleteFiles = true,
          didDeleteFiles = true,
        },
      },
    },
  })
end

return M
