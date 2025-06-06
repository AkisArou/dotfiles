return {
  "stevearc/overseer.nvim",
  config = function()
    if not vim.g.is_work then
      return
    end

    local overseer = require("overseer")
    overseer.setup()
    overseer.run_template({ name = "tsc-project-references" })
  end,
}
