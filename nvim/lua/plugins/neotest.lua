return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    {
      "mfussenegger/nvim-dap",
      keys = {
        {
          "<leader>td",
          function()
            require("neotest").run.run({ strategy = "dap" })
          end,
          desc = "Debug Nearest",
        },
      },
    },
  },
  config = function()
    vim.diagnostic.config({
      virtual_text = {
        format = function(diagnostic)
          -- Replace newline and tab characters with space for more compact diagnostics
          local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
          return message
        end,
      },
    }, vim.api.nvim_create_namespace("neotest"))

    -- stylua: ignore
    vim.keymap.set("n", "<leader>t", "", {desc = "+test"})
    -- stylua: ignore
    vim.keymap.set("n",  "<leader>tt", function() require("neotest").run.run(vim.fn.expand("%")) end, {desc = "Run File (Neotest)" })
    -- stylua: ignore
    vim.keymap.set("n",  "<leader>tT", function() require("neotest").run.run(vim.uv.cwd()) end, {desc = "Run All Test Files (Neotest)" })
    -- stylua: ignore
    vim.keymap.set("n",  "<leader>tr", function() require("neotest").run.run() end, {desc = "Run Nearest (Neotest)" })
    -- stylua: ignore
    vim.keymap.set("n",  "<leader>tl", function() require("neotest").run.run_last() end, {desc = "Run Last (Neotest)" })
    -- stylua: ignore
    vim.keymap.set("n",  "<leader>ts", function() require("neotest").summary.toggle() end, {desc = "Toggle Summary (Neotest)" })
    -- stylua: ignore
    vim.keymap.set("n",  "<leader>to", function() require("neotest").output.open({ enter = true, auto_close = true }) end, {desc = "Show Output (Neotest)" })
    -- stylua: ignore
    vim.keymap.set("n",  "<leader>tO", function() require("neotest").output_panel.toggle() end, {desc = "Toggle Output Panel (Neotest)" })
    -- stylua: ignore
    vim.keymap.set("n",  "<leader>tS", function() require("neotest").run.stop() end, {desc = "Stop (Neotest)" })
    -- stylua: ignore
    vim.keymap.set("n",  "<leader>tw", function() require("neotest").watch.toggle(vim.fn.expand("%")) end, {desc = "Toggle Watch (Neotest)" })

    require("neotest").setup({
      status = { virtual_text = true },
      output = { open_on_run = true },
      adapters = {},
    })
  end,
}
