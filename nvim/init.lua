vim.g.is_work = vim.fn.getcwd():match("nable%-solutions") ~= nil
vim.g.os_theme = os.getenv("THEME") or "tokyonight"

vim.api.nvim_create_autocmd("PackChanged", {
  callback = function(event)
    local name = event.data.spec.name

    if name == "nvim-treesitter" then
      vim.cmd("TSUpdate")
    end

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
  github("nvim-lualine/lualine.nvim"),
  github("arnamak/stay-centered.nvim"),

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

  github("axelvc/template-string.nvim"),

  github("folke/todo-comments.nvim"),

  github("mbbill/undotree"),

  github("christoomey/vim-tmux-navigator"),

  github("mg979/vim-visual-multi"),

  github("folke/which-key.nvim"),

  github("mikavilpas/yazi.nvim"),

  github("folke/lazydev.nvim"),

  github("saghen/blink.cmp"),

  github("stevearc/conform.nvim"),

  github("yioneko/nvim-vtsls"),
})

-- Instant load
require("config")
require("plugins.colorscheme")

-- Deferred load
vim.defer_fn(function()
  require("plugins.treesitter")
  require("plugins.lualine")
  require("plugins.stay-centered")
  require("plugins.vim-tmux-navigator")
  require("plugins.yazi")
  require("plugins.fzf")
  require("plugins.lsp.conform")
  require("plugins.lsp.lsp")
  require("plugins.lsp.mason")
  require("plugins.nvim-lint")
  require("plugins.lsp.blink")
  require("plugins.dap")
  require("plugins.overseer")
  require("plugins.diffview")
  require("plugins.gitsigns")
  require("plugins.grug-far")
  require("plugins.highlighturl")
  require("plugins.nvim-colorizer")
  require("plugins.neogit")
  require("plugins.mini")
  require("plugins.octo")
  require("plugins.snacks")
  require("plugins.todo-comments")
  require("plugins.undotree")
  require("plugins.which-key")
  require("plugins.template-string")

  require("custom.revive").setup({ auto = false })
end, 0)

-- Autocmd load
vim.api.nvim_create_autocmd("BufReadPost", {
  once = true,
  pattern = { "*.ts", "*.tsx", "*.js", "*.jsx" },
  callback = function()
    require("plugins.neotest")
    require("custom.pnpm").setup()
  end,
})

vim.api.nvim_create_autocmd("BufReadPost", {
  once = true,
  pattern = { "*.json", "*.jsonc", "*.yaml", "*.yml" },
  callback = function()
    require("plugins.schemastore")
  end,
})

vim.api.nvim_create_autocmd("InsertEnter", {
  once = true,
  pattern = "*",
  callback = function()
    require("plugins.neotab")
    require("plugins.vim-visual-multi")
  end,
})
