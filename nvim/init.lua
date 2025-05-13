vim.g.is_work = vim.fn.getcwd():match("nable%-solutions") ~= nil
vim.g.os_theme = os.getenv("THEME") or "tokyonight"

require("config")
require("custom.pnpm").setup()

if vim.g.is_work then
  local cwd = vim.fn.getcwd()
  local project = cwd .. "/tsconfig.json"

  require("custom.tsc").setup({
    args = "--build " .. project .. " --watch",
  })
end
