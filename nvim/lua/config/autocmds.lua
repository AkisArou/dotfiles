-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

-- Disable the concealing in some file formats
-- The default conceallevel is 3 in LazyVim
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "json", "jsonc", "markdown" },
  callback = function()
    vim.opt.conceallevel = 0
  end,
})

-- CSS variables lsp

local file_path = os.getenv("HOME")
  .. "/.asdf/installs/nodejs/20.10.0/lib/node_modules/css-variables-language-server/dist/index.js"

-- Patch lsp (null TypeError in line 249)
local com = "grep -F "
  .. '\'settings = { lookupFiles: ["**/*.less", "**/*.scss\''
  .. " "
  .. file_path
  .. " || "
  .. 'sed -i \'249i\\   settings = { lookupFiles: ["**/*.less", "**/*.scss", "**/*.sass", "**/*.css"], blacklistFolders: ["**/.cache", "**/.DS_Store", "**/.git", "**/.hg", "**/.next", "**/.svn", "**/bower_components", "**/CVS", "**/dist", "**/node_modules", "**/tests", "**/tmp"] }\' '
  .. file_path

vim.fn.system(com)
