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

-- Npm workspaces lsp
local c = vim.lsp.start_client({
  config = {
    cmd = { "npx", "npm-workspaces-lsp", "--stdio" },
  },
  name = "npm-workspaces-lsp",
  cmd = { "npx", "npm-workspaces-lsp", "--stdio" },
  root_dir = vim.loop.cwd(),
})

vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
  pattern = { "package.json" },
  callback = function()
    vim.lsp.buf_attach_client(0, c)
  end,
})
--

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

local css_variables_lsp_cmd = {
  "node",
  file_path,
  "--nolazy",
  "--stdio",
}

local c = vim.lsp.start_client({
  config = {
    cmd = css_variables_lsp_cmd,
  },
  settings = {
    lookupFiles = { "**/*.less", "**/*.scss", "**/*.sass", "**/*.css" },
    blacklistFolders = {
      "**/.cache",
      "**/.DS_Store",
      "**/.git",
      "**/.hg",
      "**/.next",
      "**/.svn",
      "**/bower_components",
      "**/CVS",
      "**/dist",
      "**/node_modules",
      "**/tests",
      "**/tmp",
    },
  },
  name = "css-variables-language-server",
  cmd = css_variables_lsp_cmd,
  root_dir = vim.loop.cwd(),
})

vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
  pattern = { "*.css", "*.scss" },
  callback = function()
    vim.lsp.buf_attach_client(0, c)
  end,
})
--
