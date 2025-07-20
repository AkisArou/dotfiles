local lint = require("lint")

vim.env.ESLINT_D_PPID = vim.fn.getpid()

local eslint = "eslint_d"

lint.linters_by_ft = {
  javascript = { eslint },
  javascriptreact = { eslint },
  typescript = { eslint },
  typescriptreact = { eslint },
  json = { eslint },
}

vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost", "InsertLeave" }, {
  group = vim.api.nvim_create_augroup("lint", { clear = true }),
  callback = function()
    -- Somehow with delay it works better
    vim.defer_fn(function()
      lint.try_lint()
    end, 50)
  end,
})
