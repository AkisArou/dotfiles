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
  "rust_analyzer",
  -- "tsgo",
})

local fzf = require("fzf-lua")

--- Sets up LSP keymaps and autocommands for the given buffer.
---@param client vim.lsp.Client
---@param bufnr integer
local function on_attach(client, bufnr)
  local map = function(mode, keys, func, desc)
    vim.keymap.set(mode, keys, func, { buffer = bufnr, desc = "LSP: " .. desc })
  end

  if client:supports_method("textDocument/documentHighlight") then
    local under_cursor_highlights_group = vim.api.nvim_create_augroup("akisarou/cursor_highlights", { clear = false })
    vim.api.nvim_create_autocmd({ "CursorHold", "InsertLeave" }, {
      group = under_cursor_highlights_group,
      desc = "Highlight references under the cursor",
      buffer = bufnr,
      callback = vim.lsp.buf.document_highlight,
    })
    vim.api.nvim_create_autocmd({ "CursorMoved", "InsertEnter", "BufLeave" }, {
      group = under_cursor_highlights_group,
      desc = "Clear highlight references",
      buffer = bufnr,
      callback = vim.lsp.buf.clear_references,
    })
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

  if client.name == "vtsls" then
    map("n", "<leader>cqi", function()
      require("vtsls").commands.remove_unused_imports(bufnr, function()
        require("conform").format({ bufnr = bufnr, async = false })
        vim.cmd("silent! w")
      end)
    end, "Remove unused imports")
  end

  if client.name == "tsgo" then
    map("n", "<leader>cqi", function()
      vim.lsp.buf.code_action({
        context = { only = { "source.removeUnusedImports" }, diagnostics = {} },
        apply = true,
      })

      require("conform").format({ bufnr = bufnr, async = false })
      vim.cmd("silent! w")
    end, "Remove unused imports")
  end
end

-- Update mappings when registering dynamic capabilities.
local register_capability = vim.lsp.handlers["client/registerCapability"]
vim.lsp.handlers["client/registerCapability"] = function(err, res, ctx)
  local client = vim.lsp.get_client_by_id(ctx.client_id)
  if not client then
    return
  end

  on_attach(client, vim.api.nvim_get_current_buf())

  return register_capability(err, res, ctx)
end

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("akisarou.lsp", {}),
  callback = function(args)
    on_attach(assert(vim.lsp.get_client_by_id(args.data.client_id)), args.buf)
  end,
})
