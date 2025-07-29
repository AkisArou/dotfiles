local M = {}

local log = require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.log")

-- Helper to get the current file or directory in netrw
local function get_netrw_path()
  local dir = vim.g.NetrwBrowserDir or vim.fn.expand("%:p:h")
  local line = vim.fn.getline(".")
  local fname = line:match("^%s*(.+)$")
  if fname and fname ~= "" then
    return vim.fn.fnamemodify(dir .. "/" .. fname, ":p")
  end
  return dir
end

function M.rename()
  local old_name = get_netrw_path()
  local new_name = vim.fn.input("Rename to: ", old_name)
  if new_name ~= "" and new_name ~= old_name then
    require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.will-rename").callback({
      old_name = old_name,
      new_name = new_name,
    })

    vim.cmd('silent! call rename("' .. old_name .. '", "' .. new_name .. '")')

    require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.did-rename").callback({
      old_name = old_name,
      new_name = new_name,
    })

    log.debug("Netrw rename: " .. old_name .. " -> " .. new_name)
  end
end

function M.delete()
  local marked_files = vim.fn["netrw#Expose"]("netrwmarkfilelist")

  local files_to_delete = marked_files ~= "n/a" and marked_files or { get_netrw_path() }

  for _, fname in pairs(files_to_delete) do
    require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.will-delete").callback({ fname = fname })

    vim.cmd('silent! call delete("' .. fname .. '")')

    require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.did-delete").callback({ fname = fname })

    log.debug("Netrw delete: " .. fname)
  end
end

function M.create()
  local dir = get_netrw_path()
  local new_name = vim.fn.input("Create file: ", dir .. "/")
  if new_name ~= "" then
    require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.will-create").callback({ fname = new_name })

    vim.cmd('silent! call writefile([], "' .. new_name .. '")')

    require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.did-create").callback({ fname = new_name })

    log.debug("Netrw create: " .. new_name)
  end
end

return M
