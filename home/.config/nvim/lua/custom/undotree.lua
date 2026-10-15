local M = {}

---@param entries vim.fn.undotree.entry[]
---@param saved vim.fn.undotree.entry[]
local function collect_saved_entries(entries, saved)
  for _, entry in ipairs(entries) do
    if entry.save then
      saved[#saved + 1] = entry
    end

    if entry.alt then
      collect_saved_entries(entry.alt, saved)
    end
  end
end

---@param bufnr integer
---@return vim.fn.undotree.entry[], vim.fn.undotree.ret
local function saved_entries(bufnr)
  local tree = vim.fn.undotree(bufnr)
  local saved = {}

  collect_saved_entries(tree.entries, saved)
  table.sort(saved, function(a, b)
    return a.save > b.save
  end)

  return saved, tree
end

---@param selected string[]
---@param entries_by_seq table<integer, vim.fn.undotree.entry>
---@return vim.fn.undotree.entry?
local function selected_entry(selected, entries_by_seq)
  local seq = selected[1] and tonumber(selected[1]:match("^(%d+)"))
  return seq and entries_by_seq[seq] or nil
end

---@param bufnr integer
---@param seq integer
---@return string[]
local function state_at(bufnr, seq)
  if not vim.api.nvim_buf_is_valid(bufnr) then
    error("the source buffer no longer exists")
  end

  local tmp_file = vim.fn.tempname()
  local tmp_undo = tmp_file .. ".undo"
  local tmp_buf
  local lines

  local ok, err = xpcall(function()
    vim.fn.writefile(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), tmp_file)
    tmp_buf = vim.fn.bufadd(tmp_file)
    vim.bo[tmp_buf].swapfile = false
    vim.fn.bufload(tmp_buf)

    vim.api.nvim_buf_call(bufnr, function()
      vim.cmd("silent wundo! " .. vim.fn.fnameescape(tmp_undo))
    end)
    vim.api.nvim_buf_call(tmp_buf, function()
      vim.cmd("silent rundo " .. vim.fn.fnameescape(tmp_undo))
      vim.cmd("noautocmd silent undo " .. seq)
      lines = vim.api.nvim_buf_get_lines(tmp_buf, 0, -1, false)
    end)
  end, debug.traceback)

  if tmp_buf and vim.api.nvim_buf_is_valid(tmp_buf) then
    pcall(vim.api.nvim_buf_delete, tmp_buf, { force = true })
  end
  vim.fn.delete(tmp_file)
  vim.fn.delete(tmp_undo)

  if not ok then
    error(err, 0)
  end

  return lines
end

local snapshot_id = 0

---@param source_buf integer
---@param entry vim.fn.undotree.entry
---@param lines string[]
---@param listed boolean
---@return integer
local function create_snapshot_buf(source_buf, entry, lines, listed)
  snapshot_id = snapshot_id + 1

  local source_name = vim.api.nvim_buf_get_name(source_buf)
  local display_name = source_name ~= "" and source_name or "[No Name]"
  local buf = vim.api.nvim_create_buf(listed, true)
  local name = ("undo://%s?save=%d&change=%d&id=%d"):format(display_name, entry.save, entry.seq, snapshot_id)

  vim.api.nvim_buf_set_name(buf, name)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].filetype = vim.bo[source_buf].filetype
  vim.bo[buf].modified = false
  vim.bo[buf].readonly = true
  vim.bo[buf].modifiable = false

  return buf
end

---@param source_buf integer
---@param entry vim.fn.undotree.entry
---@param lines string[]
local function open_snapshot(source_buf, entry, lines)
  local buf = create_snapshot_buf(source_buf, entry, lines, true)
  vim.api.nvim_set_current_buf(buf)
end

local codediff_id = 0

---@param source_buf integer
---@param entry vim.fn.undotree.entry
---@param lines string[]
local function open_diff(source_buf, entry, lines)
  codediff_id = codediff_id + 1

  local tmp_dir = vim.fn.tempname()
  local current_dir = tmp_dir .. "/current"
  local saved_dir = tmp_dir .. "/save-" .. entry.save
  local source_name = vim.api.nvim_buf_get_name(source_buf)
  local filename = source_name ~= "" and vim.fn.fnamemodify(source_name, ":t") or "buffer"
  local current_file = current_dir .. "/" .. filename
  local saved_file = saved_dir .. "/" .. filename

  local function delete_buffers()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_valid(buf) then
        local name = vim.api.nvim_buf_get_name(buf)
        if name == current_file or name == saved_file then
          pcall(vim.api.nvim_buf_delete, buf, { force = true })
        end
      end
    end
  end

  local function delete_files()
    vim.fn.delete(current_file)
    vim.fn.delete(saved_file)
    vim.fn.delete(current_dir, "d")
    vim.fn.delete(saved_dir, "d")
    vim.fn.delete(tmp_dir, "d")
  end

  local ok, err = pcall(function()
    assert(vim.fn.mkdir(current_dir, "p") == 1, "failed to create current-state directory")
    assert(vim.fn.mkdir(saved_dir, "p") == 1, "failed to create saved-state directory")
    assert(
      vim.fn.writefile(vim.api.nvim_buf_get_lines(source_buf, 0, -1, false), current_file) == 0,
      "failed to write current state"
    )
    assert(vim.fn.writefile(lines, saved_file) == 0, "failed to write saved state")

    vim.api.nvim_cmd({ cmd = "CodeDiff", args = { "file", current_file, saved_file } }, {})
  end)

  if not ok then
    delete_buffers()
    delete_files()
    error(err, 0)
  end

  local diff_tab = vim.api.nvim_get_current_tabpage()
  local group = vim.api.nvim_create_augroup("SavedUndoCodeDiff" .. codediff_id, { clear = true })
  local cleaned = false

  local function cleanup(delete_loaded_buffers)
    if cleaned then
      return
    end
    cleaned = true

    if delete_loaded_buffers then
      delete_buffers()
    end
    delete_files()
    pcall(vim.api.nvim_del_augroup_by_id, group)
  end

  vim.api.nvim_create_autocmd("User", {
    group = group,
    pattern = "CodeDiffClose",
    callback = function(event)
      if event.data and event.data.tabpage == diff_tab then
        vim.schedule(function()
          cleanup(true)
        end)
      end
    end,
  })
  vim.api.nvim_create_autocmd("TabClosed", {
    group = group,
    callback = function()
      vim.schedule(function()
        if not vim.api.nvim_tabpage_is_valid(diff_tab) then
          cleanup(true)
        end
      end)
    end,
  })
  vim.api.nvim_create_autocmd("VimLeavePre", {
    group = group,
    once = true,
    callback = function()
      cleanup(false)
    end,
  })
end

---@param source_buf integer
---@param entries_by_seq table<integer, vim.fn.undotree.entry>
---@param selected string[]
---@param action fun(source_buf: integer, entry: vim.fn.undotree.entry, lines: string[])
local function run_snapshot_action(source_buf, entries_by_seq, selected, action)
  local entry = selected_entry(selected, entries_by_seq)
  if not entry then
    vim.notify("Unable to read the selected undo state", vim.log.levels.WARN)
    return
  end

  local ok, err = pcall(function()
    action(source_buf, entry, state_at(source_buf, entry.seq))
  end)
  if not ok then
    vim.notify("Unable to open undo state: " .. tostring(err), vim.log.levels.ERROR)
  end
end

---@param opts? table
function M.open(opts)
  local fzf_lua = require("fzf-lua")
  local source_buf = vim.api.nvim_get_current_buf()
  local entries, tree = saved_entries(source_buf)

  if #entries == 0 then
    vim.notify("No saved states in this buffer's undo history", vim.log.levels.INFO)
    return
  end

  local items = {}
  local entries_by_seq = {}
  local locate_pos

  for index, entry in ipairs(entries) do
    entries_by_seq[entry.seq] = entry
    items[index] = ("%d\tsave #%d\t%s"):format(
      entry.seq,
      entry.save,
      os.date("%Y-%m-%d %H:%M:%S", entry.time)
    )
    if entry.seq == tree.seq_cur then
      locate_pos = index
    end
  end

  local picker_opts = {
    prompt = "Saved undo> ",
    previewer = "undotree",
    locate = locate_pos ~= nil,
    __locate_pos = locate_pos,
    fzf_opts = { ["--no-multi"] = true },
    keymap = { builtin = { ["<F8>"] = "toggle-preview-undo" } },
    actions = {
      ["enter"] = require("fzf-lua.actions").undo,
      ["ctrl-f"] = {
        fn = function(selected)
          run_snapshot_action(source_buf, entries_by_seq, selected, open_snapshot)
        end,
        header = "open saved state",
      },
      ["ctrl-g"] = {
        fn = function(selected)
          run_snapshot_action(source_buf, entries_by_seq, selected, open_diff)
        end,
        header = "diff current → saved",
      },
    },
  }

  fzf_lua.fzf_exec(items, vim.tbl_deep_extend("force", picker_opts, opts or {}))
end

return M
