-- Set default root marker for all clients
vim.lsp.config("*", {
  root_markers = { ".git" },
})

vim.lsp.enable({
  "cssls",
  "cssmodules_ls",
  "docker_compose_language_service",
  "dockerls",
  "html",
  "jsonls",
  "lua_ls",
  "mdx_analyzer",
  "npmls",
  "tailwindcss",
  "taplo",
  "vtsls",
  "yamlls",
})

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("akisarou.lsp", {}),
  callback = function(args)
    local bufnr = args.buf
    local keymap = vim.keymap
    local fzf = require("fzf-lua")

    local opts = { noremap = true, silent = true }

    opts.desc = "Show LSP references"
    keymap.set("n", "grr", function()
      fzf.lsp_references({
        ignore_current_line = true,
        winopts = {
          preview = {
            layout = "vertical",
          },
        },
      })
    end, opts)

    opts.desc = "Go to declaration"
    keymap.set("n", "gD", vim.lsp.buf.declaration, opts)

    opts.desc = "Show LSP definitions"
    keymap.set("n", "gd", function()
      fzf.lsp_definitions({
        jump1 = true,
        winopts = {
          preview = {
            layout = "vertical",
          },
        },
      })
    end, opts)

    opts.desc = "See available code actions"
    keymap.set({ "n", "v" }, "gra", function()
      local ommited_actions = {
        "Move to",
        "Extract to",
        "Change to parameter",
        "Add missing function declaration",
        "Add all missing function declarations",
        "Generate 'get'",
        "Add braces",
        "Remove unused",
        "Fix all detected spelling",
        "Change spelling to",
      }

      fzf.lsp_code_actions({
        filter = function(action)
          for _, value in ipairs(ommited_actions) do
            if string.find(action.title, value) then
              return false
            end
          end

          return true
        end,
      })
    end, opts)

    opts.desc = "Show buffer diagnostics"
    keymap.set("n", "<C-w><C-f>", ":FzfLua lsp_document_diagnostics<CR>", opts)

    opts.desc = "Restart LSP"
    keymap.set("n", "<leader>cl", "<cmd>LspRestart<CR>", opts)

    opts.desc = "Remove unused imports"
    keymap.set("n", "<leader>cqi", function()
      require("vtsls").commands.remove_unused_imports(bufnr, function()
        require("conform").format({ bufnr = bufnr, async = false })
        vim.cmd("silent! w")
      end)
    end, opts)
  end,
})
