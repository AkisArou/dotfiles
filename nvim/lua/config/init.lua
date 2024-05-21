require("config.options")
require("config.lazy")
require("config.autocmds")
require("config.keymaps")
require("config.filetype")

local cwd = vim.fn.getcwd()
local project = cwd .. "/tsconfig.json"

require("tsc.init").setup({
  args = "--build " .. project .. " --watch",
})
