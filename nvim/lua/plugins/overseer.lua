return {
  "stevearc/overseer.nvim",
  enabled = false,
  config = function()
    local overseer = require("overseer")

    overseer.setup({
      strategy = {
        "jobstart",
        open_on_start = false,
      },
    })

    local cwd = vim.fn.getcwd()
    local is_nable = cwd:match("nable%-solutions")

    if is_nable then
      overseer.run_template({ name = "tsc-project-references" })
    end
  end,
}
