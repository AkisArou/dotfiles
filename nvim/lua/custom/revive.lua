local M = {}

local session_file = vim.fn.stdpath("state") .. "/revive.json"

local function load_sessions()
  local f = io.open(session_file, "r")
  if not f then
    return {}
  end
  local content = f:read("*a")
  f:close()
  local ok, sessions = pcall(vim.json.decode, content)
  return ok and sessions or {}
end

local function save_sessions(sessions)
  local dir = vim.fn.fnamemodify(session_file, ":h")
  vim.fn.mkdir(dir, "p")
  local f = io.open(session_file, "w")
  if not f then
    return
  end
  f:write(vim.json.encode(sessions))
  f:close()
end

-- Save session when Neovim exits
function M.save_session()
  local cwd = vim.fn.getcwd()
  local sessions = load_sessions()
  sessions[cwd] = {}

  for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(bufnr) and vim.bo[bufnr].buftype == "" then
      local name = vim.api.nvim_buf_get_name(bufnr)
      if name ~= "" then
        table.insert(sessions[cwd], name)
      end
    end
  end

  save_sessions(sessions)
end

-- Load session manually
function M.load_session()
  local cwd = vim.fn.getcwd()
  local sessions = load_sessions()
  local files = sessions[cwd]
  if not files then
    print("No session for current directory")
    return
  end
  for _, file in ipairs(files) do
    if vim.fn.filereadable(file) == 1 then
      vim.cmd("edit " .. vim.fn.fnameescape(file))
    end
  end
end

-- Setup keymap and autocmd
function M.setup()
  vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = M.save_session,
  })

  vim.keymap.set("n", "<leader>sl", M.load_session, { desc = "Load last session buffers" })
end

return M
