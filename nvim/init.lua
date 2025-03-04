vim.g.is_work = vim.fn.getcwd():match("nable%-solutions") ~= nil

require("config")
require("custom")
