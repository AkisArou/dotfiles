return {
  "mfussenegger/nvim-lint",
  enabled = false,
  config = function()
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

    local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })

    vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost", "InsertLeave" }, {
      group = lint_augroup,
      callback = function()
        lint.try_lint()
      end,
    })
  end,
}
