vim.cmd("packadd nvim.difftool")
vim.cmd("packadd nvim.undotree")

vim.api.nvim_create_autocmd("PackChanged", {
  callback = function(event)
    local name = event.data.spec.name

    if name == "blink.cmp" then
      local plugin_path = vim.fn.stdpath("data") .. "/site/pack/core/opt/blink.cmp"

      vim.notify("\nBuilding blink.cmp at " .. plugin_path, vim.log.levels.INFO)
      local result = vim.system({ "cargo", "build", "--release" }, { cwd = plugin_path }):wait()

      if result.code == 0 then
        vim.notify("\nBuilding blink.cmp done", vim.log.levels.INFO)
      else
        vim.notify("\nBuilding blink.cmp failed", vim.log.levels.ERROR)
      end
    end
  end,
})

---@param value string|vim.pack.Spec
---@return string|vim.pack.Spec
local function gh(value)
  if type(value) == "string" then
    return "https://github.com/" .. value
  end

  value.src = "https://github.com/" .. value.src
  return value
end

vim.pack.add({
  gh("j-hui/fidget.nvim"),
  gh("folke/tokyonight.nvim"),
  gh("nvim-lua/plenary.nvim"),
  gh("nvim-lualine/lualine.nvim"),
  gh("arnamak/stay-centered.nvim"),
  gh("fei6409/log-highlight.nvim"),
  gh("3rd/image.nvim"),

  gh("Bilal2453/luvit-meta"),

  gh("RubixDev/mason-update-all"),
  gh("williamboman/mason-lspconfig.nvim"),
  gh("mason-org/mason.nvim"),
  gh("jay-babu/mason-nvim-dap.nvim"),

  gh({ src = "nvim-treesitter/nvim-treesitter", version = "main" }),
  gh({ src = "nvim-treesitter/nvim-treesitter-textobjects", version = "main" }),
  gh("nvim-treesitter/nvim-treesitter-context"),
  gh("windwp/nvim-ts-autotag"),
  gh("folke/ts-comments.nvim"),

  gh("rcarriga/nvim-dap-ui"),
  gh("nvim-neotest/nvim-nio"),
  gh("mfussenegger/nvim-dap"),

  gh("ibhagwan/fzf-lua"),

  gh("lewis6991/gitsigns.nvim"),

  gh("MagicDuck/grug-far.nvim"),

  gh("itchyny/vim-highlighturl"),

  gh("echasnovski/mini.cursorword"),
  gh("echasnovski/mini.surround"),
  gh("echasnovski/mini.pairs"),
  gh("echasnovski/mini.ai"),

  gh("kawre/neotab.nvim"),

  gh("nvim-neotest/nvim-nio"),
  gh("antoinemadec/FixCursorHold.nvim"),
  gh("nvim-neotest/neotest"),
  gh("markandrus/neotest-node-test-runner"),

  gh("catgoose/nvim-colorizer.lua"),

  gh("mfussenegger/nvim-lint"),

  gh("stevearc/overseer.nvim"),

  gh("b0o/SchemaStore.nvim"),

  gh("folke/snacks.nvim"),

  gh("axelvc/template-string.nvim"),

  gh("folke/todo-comments.nvim"),

  gh("christoomey/vim-tmux-navigator"),

  gh("mg979/vim-visual-multi"),

  gh("folke/which-key.nvim"),

  gh("stevearc/oil.nvim"),
  gh("JezerM/oil-lsp-diagnostics.nvim"),
  gh("refractalize/oil-git-status.nvim"),

  gh("folke/lazydev.nvim"),

  gh("saghen/blink.cmp"),
  gh("Kaiser-Yang/blink-cmp-git"),

  gh("stevearc/conform.nvim"),

  gh("yioneko/nvim-vtsls"),

  gh("folke/persistence.nvim"),

  gh("MunifTanjim/nui.nvim"),
  gh("esmuellert/codediff.nvim"),
  gh("pwntester/octo.nvim"),
  gh("NeogitOrg/neogit"),
})

-- Instant load
require("plugins.colorscheme")
require("plugins.lualine")

-- Deferred load
vim.schedule(function()
  require("plugins.treesitter")
  require("plugins.stay-centered")
  require("plugins.vim-tmux-navigator")
  require("plugins.oil")
  require("plugins.fzf")
  require("plugins.lsp.conform")
  require("plugins.lsp.lsp")
  require("plugins.lsp.mason")
  require("plugins.nvim-lint")
  require("plugins.lsp.blink")
  require("plugins.dap")
  require("plugins.overseer")
  require("plugins.gitsigns")
  require("plugins.grug-far")
  require("plugins.highlighturl")
  require("plugins.nvim-colorizer")
  require("plugins.mini")
  require("plugins.snacks")
  require("plugins.todo-comments")
  require("plugins.which-key")
  require("plugins.template-string")
  require("plugins.fidget")
  require("plugins.persistence")
  require("plugins.octo")
  require("plugins.codediff")
  require("plugins.neogit")
end)

-- Autocmd load
vim.api.nvim_create_autocmd("BufReadPost", {
  once = true,
  pattern = { "*test.ts", "*test.tsx", "*test.js", "*test.jsx" },
  callback = function()
    require("plugins.neotest")
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
