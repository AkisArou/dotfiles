local M = {}

-- Default configuration
local config = {
  ignore_paths = {},
  ignore_files = {},
  session_dir = vim.fn.stdpath("data") .. "/sessionizer/",
}

-- Function to convert the current working directory path to a valid filename
local function cwd_to_filename()
  local cwd = vim.fn.getcwd()
  return cwd:gsub("[/:]", "_")
end

-- Function to get the full path to the session file
local function get_session_file()
  return config.session_dir .. cwd_to_filename() .. ".txt"
end

-- Function to check if a path should be ignored
local function is_ignored_path(path)
  for _, ignore_path in ipairs(config.ignore_paths) do
    if path:find(ignore_path) then
      return true
    end
  end
  return false
end

-- Function to check if a file should be ignored
local function is_ignored_file(filename)
  for _, ignore_file in ipairs(config.ignore_files) do
    if filename:find(ignore_file) then
      return true
    end
  end
  return false
end

-- Function to save the current session
function M.save_session()
  if is_ignored_path(vim.fn.getcwd()) then
    return
  end

  -- Create session directory if it doesn't exist
  if vim.fn.isdirectory(config.session_dir) == 0 then
    vim.fn.mkdir(config.session_dir, "p")
  end

  local buffers = vim.api.nvim_list_bufs()
  local open_files = {}
  local current_buf = vim.api.nvim_get_current_buf()
  local current_buf_name = vim.api.nvim_buf_get_name(current_buf)

  for _, buf in ipairs(buffers) do
    if vim.api.nvim_buf_is_loaded(buf) then
      local filename = vim.api.nvim_buf_get_name(buf)
      if filename ~= "" and not is_ignored_file(filename) then
        table.insert(open_files, filename)
      end
    end
  end

  -- Debugging output
  print("Saving session to: " .. get_session_file())
  print("Current buffer name: " .. current_buf_name)

  local session_path = get_session_file()
  local file = io.open(session_path, "w")
  if file then
    -- Write focused buffer at the start with a marker
    file:write("FOCUSED_BUFFER:" .. current_buf_name .. "\n")

    -- Write the list of open files
    for _, filename in ipairs(open_files) do
      file:write(filename .. "\n")
    end

    file:close()
    print("Session saved successfully.")
  else
    print("Error: Could not open session file for writing.")
  end
end

-- Function to refresh syntax for all open buffers
local function refresh_syntax(bufnrs)
  for _, bufnr in ipairs(bufnrs) do
    if vim.api.nvim_buf_is_valid(bufnr) and vim.api.nvim_buf_is_loaded(bufnr) then
      vim.api.nvim_set_current_buf(bufnr)
      if vim.treesitter then
        vim.cmd("edit") -- This reattaches Treesitter
        -- else
        --   vim.cmd("syntax sync fromstart")
      end
    end
  end
end

-- Function to load the previous session
function M.load_session()
  local session_path = get_session_file()
  local file = io.open(session_path, "r")

  if file then
    local bufnrs = {}
    local last_focused_file = nil

    -- Debugging output
    print("Loading session from: " .. session_path)

    -- Read the session file
    local first_line = file:read()
    if first_line and first_line:find("FOCUSED_BUFFER:") then
      last_focused_file = first_line:gsub("FOCUSED_BUFFER:", "")
      print("Last focused file: " .. last_focused_file)
    end

    -- Read the remaining lines for open files
    for line in file:lines() do
      if vim.fn.filereadable(line) == 1 then
        vim.cmd("edit " .. vim.fn.fnameescape(line))
        table.insert(bufnrs, vim.api.nvim_get_current_buf())
      end
    end
    file:close()

    vim.defer_fn(function()
      refresh_syntax(bufnrs)
      if last_focused_file then
        for _, bufnr in ipairs(bufnrs) do
          if vim.api.nvim_buf_get_name(bufnr) == last_focused_file then
            vim.api.nvim_set_current_buf(bufnr)
            break
          end
        end
      end
    end, 0)
  else
    print("Error: Could not open session file for reading.")
  end
end

-- Setup function to initialize the plugin with user-provided configuration
function M.setup(user_config)
  config = vim.tbl_extend("force", config, user_config or {})

  -- Automatically save the session when Neovim exits
  vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = M.save_session,
  })

  -- Automatically load the session when Neovim starts
  vim.api.nvim_create_autocmd("VimEnter", {
    callback = M.load_session,
  })
end

return M
