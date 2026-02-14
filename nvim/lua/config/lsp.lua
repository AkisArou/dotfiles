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
  "oxlint",
  "oxfmt",
  -- "tsgo",
})

local fzf = require("fzf-lua")

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("akisarou.lsp", {}),
  callback = function(args)
    local map = function(mode, keys, func, desc)
      vim.keymap.set(mode, keys, func, { buffer = args.buf, desc = "LSP: " .. desc })
    end

    map("n", "grr", function()
      fzf.lsp_references({
        ignore_current_line = true,
      })
    end, "Show references")

    map("n", "gD", vim.lsp.buf.declaration, "Go to declaration")

    map("n", "gd", function()
      fzf.lsp_definitions({
        jump1 = true,
      })
    end, "Go definitions")

    map("n", "gri", fzf.lsp_implementations, "Go implementations")

    map("n", "grt", fzf.lsp_typedefs, "Go type definitions")

    map({ "n", "v" }, "gra", function()
      local ommited_actions = {
        "Inline variable",
        "Move to",
        "Extract",
        "Change to parameter",
        "Convert",
        "Add missing function declaration",
        "Add all missing function declarations",
        "Generate 'get'",
        "Add braces",
        "Remove braces",
        "Remove unused",
        "Fix all detected spelling",
        "Change spelling to",
        "Convert named export",
        "Disable react/jsx-no-undef",
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
    end, "Code actions")

    map("n", "<C-w><C-f>", ":FzfLua lsp_document_diagnostics<CR>", "Show buffer diagnostics")

    map("n", "<leader>cl", "<cmd>LspRestart<CR>", "Restart LSP")

    map("n", "<leader>cqi", function()
      require("vtsls").commands.remove_unused_imports(args.buf, function()
        require("conform").format({ bufnr = args.buf, async = false })
        vim.cmd("silent! w")
      end)
    end, "Remove unused imports")
  end,
})
