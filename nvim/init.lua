vim.g.is_work = vim.fn.getcwd():match("nable%-solutions") ~= nil
vim.g.os_theme = os.getenv("THEME") or "tokyonight"

vim.api.nvim_create_autocmd("PackChanged", {
  callback = function(event)
    local name = event.data.spec.name

    if name == "blink.cmp" then
      local plugin_path = vim.fn.stdpath("data") .. "/site/pack/core/opt/blink.cmp"

      vim.notify("Building blink.cmp at " .. plugin_path, vim.log.levels.INFO)
      local result = vim.system({ "cargo", "build", "--release" }, { cwd = plugin_path }):wait()

      if result.code == 0 then
        vim.notify("Building blink.cmp done", vim.log.levels.INFO)
      else
        vim.notify("Building blink.cmp failed", vim.log.levels.ERROR)
      end
    end
  end,
})

local github = function(path)
  return "https://github.com/" .. path
end

vim.pack.add({
  github("folke/tokyonight.nvim"),
  github("nvim-lua/plenary.nvim"),
  github("Bilal2453/luvit-meta"),
  github("nvim-tree/nvim-web-devicons"),

  github("RubixDev/mason-update-all"),
  github("williamboman/mason-lspconfig.nvim"),
  github("mason-org/mason.nvim"),
  github("jay-babu/mason-nvim-dap.nvim"),

  { src = github("nvim-treesitter/nvim-treesitter"), version = "main" },
  { src = github("nvim-treesitter/nvim-treesitter-textobjects"), version = "main" },
  github("nvim-treesitter/nvim-treesitter-context"),
  github("windwp/nvim-ts-autotag"),
  github("folke/ts-comments.nvim"),

  github("rcarriga/nvim-dap-ui"),
  github("nvim-neotest/nvim-nio"),
  github("mfussenegger/nvim-dap"),

  github("sindrets/diffview.nvim"),

  github("ibhagwan/fzf-lua"),

  github("NeogitOrg/neogit"),

  github("pwntester/octo.nvim"),

  github("lewis6991/gitsigns.nvim"),

  github("MagicDuck/grug-far.nvim"),

  github("itchyny/vim-highlighturl"),

  github("nvim-lualine/lualine.nvim"),

  github("echasnovski/mini.cursorword"),
  github("echasnovski/mini.surround"),
  github("echasnovski/mini.pairs"),
  github("echasnovski/mini.ai"),

  github("kawre/neotab.nvim"),

  github("nvim-neotest/nvim-nio"),
  github("antoinemadec/FixCursorHold.nvim"),
  github("nvim-neotest/neotest"),
  github("markandrus/neotest-node-test-runner"),

  github("catgoose/nvim-colorizer.lua"),

  github("mfussenegger/nvim-lint"),

  github("stevearc/overseer.nvim"),

  github("b0o/SchemaStore.nvim"),

  github("folke/snacks.nvim"),

  github("arnamak/stay-centered.nvim"),

  github("axelvc/template-string.nvim"),

  github("folke/todo-comments.nvim"),

  github("mbbill/undotree"),

  github("christoomey/vim-tmux-navigator"),

  { src = github("mg979/vim-visual-multi"), version = "master" },

  github("folke/which-key.nvim"),

  github("mikavilpas/yazi.nvim"),

  github("folke/lazydev.nvim"),

  github("saghen/blink.cmp"),

  github("stevearc/conform.nvim"),

  github("yioneko/nvim-vtsls"),
})

require("config")
require("custom.revive").setup({ auto = false })
require("custom.pnpm").setup()

require("plugins.lsp.blink")
require("plugins.lsp.conform")
require("plugins.lsp.lsp")
require("plugins.lsp.mason")
require("plugins.colorscheme")
require("plugins.dap")
require("plugins.diffview")
require("plugins.fzf")
require("plugins.gitsigns")
require("plugins.grug-far")
require("plugins.highlighturl")
require("plugins.lualine")
require("plugins.mini")
require("plugins.neogit")
require("plugins.neotab")
require("plugins.neotest")
require("plugins.nvim-colorizer")
require("plugins.nvim-lint")
require("plugins.octo")
require("plugins.overseer")
require("plugins.schemastore")
require("plugins.snacks")
require("plugins.stay-centered")
require("plugins.template-string")
require("plugins.todo-comments")
require("plugins.treesitter")
require("plugins.undotree")
require("plugins.vim-tmux-navigator")
require("plugins.vim-visual-multi")
require("plugins.which-key")
require("plugins.yazi")
