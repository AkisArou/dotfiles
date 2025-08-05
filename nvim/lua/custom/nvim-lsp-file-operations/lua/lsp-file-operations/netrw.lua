local M = {}

local log = require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.log")

local function get_netrw_file_path()
  local dir = vim.g.NetrwBrowserDir or vim.fn.expand("%:p:h")
  local line = vim.fn.getline(".")
  local fname = line:match("^%s*(.+)$")
  if fname and fname ~= "" then
    return vim.fn.fnamemodify(dir .. "/" .. fname, ":p")
  end
  return dir
end

local function get_netrw_dir_path()
  local file_path = get_netrw_file_path()
  return vim.fn.fnamemodify(file_path, ":h")
end

function M.rename()
  local old_name = get_netrw_file_path()
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
  local response = nil

  local marked_files = vim.fn["netrw#Expose"]("netrwmarkfilelist")
  local files = marked_files ~= "n/a" and marked_files or { get_netrw_file_path() }

  local to_delete = {}

  for _, fname in ipairs(files) do
    if response == "a" then
      table.insert(to_delete, fname)
    elseif response == "q" then
      break
    else
      local answer =
        vim.fn.input(string.format("Confirm deletion of file %s [{y(es)},n(o),a(ll),q(uit)]: ", fname)):lower()

      if answer == "a" then
        response = "a"
        table.insert(to_delete, fname)
      elseif answer == "q" then
        response = "q"
        break
      elseif answer == "y" then
        table.insert(to_delete, fname)
      end
    end
  end

  for _, fname in ipairs(to_delete) do
    require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.will-delete").callback({ fname = fname })

    vim.fn.delete(fname, "rf")

    require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.did-delete").callback({ fname = fname })

    log.debug("Netrw delete: " .. fname)
  end

  -- Refresh netrw
  vim.cmd("edit")
end

function M.create()
  local dir = get_netrw_dir_path()
  local new_name = vim.fn.input("Create file: ", dir .. "/")

  if new_name ~= "" then
    require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.will-create").callback({ fname = new_name })

    -- Create the file using Lua
    local file, err = io.open(new_name, "w") -- "w" mode creates or truncates the file
    if not file then
      vim.notify("Error creating file: " .. err, vim.log.levels.ERROR)
      return
    end
    file:close()

    require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.did-create").callback({ fname = new_name })

    vim.cmd("edit")

    -- Focus the file in netrw
    local fname_only = vim.fn.fnamemodify(new_name, ":t")
    vim.fn.search("^\\s*" .. vim.fn.escape(fname_only, [[\/.*~$^]]), "cw")

    log.debug("Netrw create (Lua): " .. new_name)
  end
end

return M
