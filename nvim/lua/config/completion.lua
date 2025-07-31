local function pumvisible()
  return tonumber(vim.fn.pumvisible()) ~= 0
end

local chars = {}
for i = 32, 126 do
  table.insert(chars, string.char(i))
end

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
    local bufnr = args.buf

    if not client:supports_method("textDocument/completion") then
      return
    end

    client.server_capabilities.completionProvider.triggerCharacters = chars

    -- Enable completion and configure keybindings.
    vim.lsp.completion.enable(true, client.id, bufnr, { autotrigger = true })

    -- Use enter to accept completions.
    vim.keymap.set("i", "<C-e>", function()
      return pumvisible() and "<C-y>" or "<cr>"
    end, { expr = true })

    -- Buffer completions.
    vim.keymap.set("i", "<C-u>", "<C-x><C-n>", { desc = "Buffer completions" })
  end,
})
