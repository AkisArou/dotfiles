local lint = require("lint")

local js_linter = (function()
  -- oxlint
  local oxlint_bin = vim.fn.fnamemodify("./node_modules/.bin/" .. "oxlint", ":p")
  local oxlint_stat = vim.loop.fs_stat(oxlint_bin)

  if oxlint_stat then
    local oxlint = require("lint").linters.oxlint

    oxlint.cmd = function()
      return oxlint_bin
    end

    return { "oxlint" }
  end

  -- eslint_d
  local eslint_stat = vim.loop.fs_stat(vim.fn.fnamemodify("./node_modules/.bin/" .. "eslint", ":p"))

  if eslint_stat then
    vim.env.ESLINT_D_PPID = vim.fn.getpid()

    return { "eslint_d" }
  end

  return {}
end)()

lint.linters_by_ft = {
  javascript = js_linter,
  javascriptreact = js_linter,
  typescript = js_linter,
  typescriptreact = js_linter,
}

vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost", "InsertLeave" }, {
  group = vim.api.nvim_create_augroup("lint", { clear = true }),
  callback = function()
    lint.try_lint()
  end,
})
