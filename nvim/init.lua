vim.g.is_work = vim.fn.getcwd():match("nable%-solutions") ~= nil
vim.g.os_theme = os.getenv("THEME") or "onedark"

require("config")
require("custom")
