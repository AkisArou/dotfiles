require("config.options")
require("config.lazy")
require("config.autocmds")
require("config.keymaps")
require("config.filetype")

-- local cwd = vim.fn.getcwd()
--
-- if cwd:match("nable%-solutions") then
--   local project = cwd .. "/tsconfig.json"
--
--   require("tsc.init").setup({
--     args = "--build " .. project .. " --watch",
--   })
-- end
