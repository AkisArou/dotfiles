require("config.options")
require("config.lazy")
require("config.autocmd")
require("config.keymap")
require("config.filetype")
require("config.netrw")

local cwd = vim.fn.getcwd()

if cwd:match("nable%-solutions") then
  local project = cwd .. "/tsconfig.json"

  require("custom.tsc.init").setup({
    args = "--build " .. project .. " --watch",
  })
end
