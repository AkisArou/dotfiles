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
    local map = function(keys, func, desc, mode)
      mode = mode or "n"
      vim.keymap.set(mode, keys, func, { buffer = args.buf, desc = "LSP: " .. desc })
    end

    local fzf = require("fzf-lua")

    map("grr", function()
      fzf.lsp_references({
        ignore_current_line = true,
        winopts = {
          preview = {
            layout = "vertical",
          },
        },
      })
    end, "Show references")

    map("gD", vim.lsp.buf.declaration, "Go to declaration")

    map("gd", function()
      fzf.lsp_definitions({
        jump1 = true,
        winopts = {
          preview = {
            layout = "vertical",
          },
        },
      })
    end, "Show definitions")

    map("gra", function()
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
    end, "Code actions", { "n", "v" })

    map("<C-w><C-f>", ":FzfLua lsp_document_diagnostics<CR>", "Show buffer diagnostics")

    map("<leader>cl", "<cmd>LspRestart<CR>", "Restart LSP")

    map("<leader>cqi", function()
      require("vtsls").commands.remove_unused_imports(args.buf, function()
        require("conform").format({ bufnr = args.buf, async = false })
        vim.cmd("silent! w")
      end)
    end, "Remove unused imports")
  end,
})
