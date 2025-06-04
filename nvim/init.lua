vim.g.is_work = vim.fn.getcwd():match("nable%-solutions") ~= nil
vim.g.os_theme = os.getenv("THEME") or "tokyonight"

require("config")
require("custom.revive").setup({ auto = false })
require("custom.pnpm").setup()

if vim.g.is_work then
  require("custom.tsc").setup({
    npm_script = "ts:watch",
  })
end
