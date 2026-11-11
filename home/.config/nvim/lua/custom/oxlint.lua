local M = {}

--- Applies oxlint's safe fixes to the buffer.
---@param bufnr integer
M.fix_all = function(bufnr)
  local client = vim.lsp.get_clients({ bufnr = bufnr, name = "oxlint" })[1]

  if not client then
    return
  end

  local text_document = vim.lsp.util.make_text_document_params(bufnr)

  -- A server with pull diagnostics lints a buffer only when they are pulled, which Neovim does a
  -- moment after an edit, and fixes come from that lint. Pull now, so they cover the latest edits.
  if client:supports_method("textDocument/diagnostic", bufnr) then
    client:request_sync("textDocument/diagnostic", { textDocument = text_document }, 1000, bufnr)
  end

  local response = client:request_sync("textDocument/codeAction", {
    textDocument = text_document,
    range = {
      start = { line = 0, character = 0 },
      ["end"] = { line = vim.api.nvim_buf_line_count(bufnr), character = 0 },
    },
    context = { only = { "source.fixAll.oxc" }, diagnostics = {} },
  }, 1000, bufnr)

  for _, action in ipairs(response and response.result or {}) do
    if action.edit then
      vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding)
    end
  end
end

return M
