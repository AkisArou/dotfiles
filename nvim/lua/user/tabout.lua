local M = {
  "abecodes/tabout.nvim",
  branch = "master",
	event = "BufEnter"
}

function M.config() 
  require('tabout').setup {
    tabkey = "",
    backwards_tabkey = "",
    exclude = {"qf", "NvimTree", "toggleterm", "TelescopePrompt", "alpha", "netrw"}
  }
end

return M
