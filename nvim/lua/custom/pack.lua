---@alias GitStatusResult GitStatusError | GitStatusClean | GitStatusBehind | GitStatusPending | GitStatusInstalling

---@class GitStatusPending
---@field type "pending"
---@field message string

---@class GitStatusError
---@field type "error"
---@field message string

---@class GitStatusClean
---@field type "clean"

---@class GitStatusBehind
---@field type "behind"
---@field count integer
---@field commits string[]

---@class GitStatusInstalling
---@field type "installing"
---@field count integer
---@field commits string[]

--- @class Item: vim.pack.PlugData
--- @field result GitStatusResult

local api = vim.api

local function run_job(cmd)
  return coroutine.running(),
    function(callback)
      local output = {}
      local error_output = {}

      vim.fn.jobstart(cmd, {
        stdout_buffered = true,
        stderr_buffered = true,
        on_stdout = function(_, data)
          if data and type(data) == "table" then
            output = data
          end
        end,
        on_stderr = function(_, data)
          if data and type(data) == "table" then
            error_output = data
          end
        end,
        on_exit = function(_, exit_code)
          callback(table.concat(output, "\n"), table.concat(error_output, "\n"), exit_code)
        end,
      })
    end
end

---@param path string
---@param target_branch string
---@param callback fun(result: GitStatusResult)
local function get_git_status_async(path, target_branch, callback)
  coroutine.wrap(function()
    local remote_ref = "origin/" .. target_branch

    -- Fetch
    local co = coroutine.running()
    local _, fetch_job = run_job({ "git", "-C", path, "fetch", "origin", target_branch })
    fetch_job(function(out, err, code)
      coroutine.resume(co, out, err, code)
    end)
    local _, _, fetch_ec = coroutine.yield()

    if fetch_ec ~= 0 then
      callback({
        type = "error",
        message = "❌ Git fetch error",
      })
      return
    end

    -- Rev-parse
    local _, rev_parse_job = run_job({ "git", "-C", path, "rev-parse", "--verify", remote_ref })
    rev_parse_job(function(out, err, code)
      coroutine.resume(co, out, err, code)
    end)
    local _, _, rev_ec = coroutine.yield()

    if rev_ec ~= 0 then
      callback({
        type = "error",
        message = string.format("🔍 No remote branch '%s'", target_branch),
      })
      return
    end

    -- Log
    local _, log_job = run_job({ "git", "-C", path, "log", "--oneline", "HEAD.." .. remote_ref })
    log_job(function(out, err, code)
      coroutine.resume(co, out, err, code)
    end)
    local log_output, _, log_ec = coroutine.yield()

    --- @type GitStatusResult
    local result
    if log_ec == 0 and log_output then
      log_output = log_output:gsub("\n$", "")
      if log_output == "" then
        result = {
          type = "clean",
          message = "✅ Up to date",
        }
      else
        local commit_lines = vim.split(log_output, "\n", { trimempty = true })
        local commit_count = #commit_lines
        result = {
          type = "behind",
          message = string.format("%d pending commit%s", commit_count, commit_count == 1 and "" or "s"),
          commits = commit_lines,
        }
      end
    else
      result = {
        type = "error",
        message = "❌ Git log error",
      }
    end

    callback(result)
  end)()
end

--- @param items Item[]
--- @param buf integer
local function create_popup(items, buf)
  local lines = {}

  local help_line = "📋 Keybindings:"
    .. "  q - Close popup"
    .. "  U - Update all packages"
    .. "  u - Update package under cursor"
    .. "  r - Refresh git status"
    .. "  g - Show git log for package under cursor"

  table.insert(lines, help_line)
  table.insert(lines, "")

  for _, item in ipairs(items) do
    local active_status = item.active and "✅ Active" or "❌ Inactive"
    local name = item.spec.name or "Unknown"
    local src = item.spec.src or ""
    local path = item.path or ""
    local version = item.spec.version or ""
    local result = item.result

    table.insert(lines, string.format("🔹 %s [%s]", name, active_status))
    table.insert(lines, "    Source: " .. src)
    table.insert(lines, "    Path: " .. path)
    table.insert(lines, "    Version: " .. version)
    table.insert(lines, "    " .. result.message)

    if result.type == "behind" then
      for _, value in pairs(result.commits) do
        table.insert(lines, "    " .. value)
      end
    end

    table.insert(lines, "")
  end

  -- Create window
  api.nvim_open_win(buf, true, {
    relative = "editor",
    width = math.floor(vim.o.columns * 0.8),
    height = math.floor(vim.o.lines * 0.7),
    col = math.floor(vim.o.columns * 0.1),
    row = math.floor(vim.o.lines * 0.15),
    style = "minimal",
    border = "rounded",
  })

  api.nvim_buf_set_name(buf, "Package Status")
  api.nvim_buf_set_option(buf, "filetype", "text")

  api.nvim_buf_set_keymap(buf, "n", "q", ":close<CR>", { nowait = true, noremap = true, silent = true })

  api.nvim_buf_set_option(buf, "modifiable", true)
  api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  api.nvim_buf_set_option(buf, "modifiable", false)
end

--- @param pg vim.pack.PlugData
--- @param result GitStatusResult
--- @return Item
local function to_item(pg, result)
  return vim.tbl_extend("force", pg, { result = result })
end

local function run()
  local buf = api.nvim_create_buf(false, true)

  ---@type Item[]
  local pack_info = {}

  for i, value in pairs({ vim.pack.get()[47] }) do
    pack_info[i] = to_item(value, {
      type = "pending",
      message = "🔄 Checking...",
    })
  end

  create_popup(pack_info, buf)

  for i, item in ipairs(pack_info) do
    local target_branch = item.spec.version
    get_git_status_async(item.path, target_branch, function(result)
      local new_item = pack_info[i]
      new_item.result = result

      pack_info[i] = new_item

      create_popup(pack_info, buf)
    end)
  end
end

-- Optional: Command to launch
vim.api.nvim_create_user_command("PackPopup", run, {})
