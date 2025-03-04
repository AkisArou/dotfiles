require("config.options")
require("config.lazy")
require("config.autocmd")
require("config.keymap")
require("config.filetype")
require("config.netrw")

if vim.g.is_work then
  local cwd = vim.fn.getcwd()
  local project = cwd .. "/tsconfig.json"

  require("custom.tsc.init").setup({
    args = "--build " .. project .. " --watch",
  })
end
