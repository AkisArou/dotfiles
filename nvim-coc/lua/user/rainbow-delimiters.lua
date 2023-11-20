local M = {
	"HiPhish/rainbow-delimiters.nvim",
	event = "BufEnter",
  branch = "master"
}

function M.config() 
  -- This module contains a number of default definitions
  local rainbow_delimiters = require 'rainbow-delimiters'

  vim.g.rainbow_delimiters = {
      strategy = {
          [''] = rainbow_delimiters.strategy['global'],
          commonlisp = rainbow_delimiters.strategy['local'],
      },
      query = {
          [''] = 'rainbow-delimiters',
          lua = 'rainbow-blocks',
      },
      highlight = {
          'RainbowDelimiterYellow',
          'RainbowDelimiterBlue',
          'RainbowDelimiterOrange',
          'RainbowDelimiterGreen',
          'RainbowDelimiterViolet',
          'RainbowDelimiterCyan',
      },
      blacklist = {'c', 'cpp'},
  }
end

return M
