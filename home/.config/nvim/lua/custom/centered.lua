-- Keep the cursor line centred in the window: stay-centered.nvim in one file.
-- https://github.com/arnamak/stay-centered.nvim
-- MIT License, Copyright (c) 2021 Mak Arnautovic
--
-- Centring scrolls past the end of a buffer, so the window's bottom fills
-- with empty rows. Buffers of the filetypes in no_bottom_padding keep their
-- last line at the window's bottom instead, as a terminal does.

local M = {}

M.cfg = {
  -- Filetypes left alone.
  skip_filetypes = {},
  -- Filetypes whose last line stays at the window's bottom.
  no_bottom_padding = {},
  -- Takes precedence over active.
  enabled = true,
  -- For pausing the behaviour internally.
  active = true,
  -- Scrolling (<C-e>, <C-y>, the mouse) moves the cursor without centring.
  allow_scroll_move = true,
  -- Pause while the left mouse button is down, for natural mouse selection.
  disable_on_mouse = true,
}

local function center(insert)
  local cfg = M.cfg
  if not cfg.enabled or not cfg.active or vim.tbl_contains(cfg.skip_filetypes, vim.bo.filetype) then
    return
  end

  local line = vim.api.nvim_win_get_cursor(0)[1]
  if vim.b.last_line == nil then
    vim.b.last_line = line
  end

  -- The cursor moved because the window scrolled.
  if cfg.allow_scroll_move then
    local top = vim.fn.line("w0") + vim.o.scrolloff
    local bottom = vim.fn.line("w$") - vim.o.scrolloff
    if (line <= top and line > vim.b.last_line) or (line >= bottom and line < vim.b.last_line) then
      vim.b.last_line = line
      return
    end
  end

  if line ~= vim.b.last_line then
    local column = vim.fn.getcurpos()[3]
    vim.cmd("norm! zz")
    if vim.tbl_contains(cfg.no_bottom_padding, vim.bo.filetype) and vim.fn.line("w$") == vim.fn.line("$") then
      -- zb puts the last line at the bottom; then the cursor goes back.
      local cursor = vim.api.nvim_win_get_cursor(0)
      vim.api.nvim_win_set_cursor(0, { vim.fn.line("$"), 0 })
      vim.cmd("norm! zb")
      vim.api.nvim_win_set_cursor(0, cursor)
    end
    vim.b.last_line = line
    if insert then
      vim.fn.cursor({ line, column })
    end
  end
end

local left_mouse = vim.keycode("<LeftMouse>")
local left_release = vim.keycode("<LeftRelease>")

local function on_mouse(key)
  if not M.cfg.enabled then
    return
  end
  if key == left_mouse then
    M.deactivate()
  elseif key == left_release then
    vim.b.last_line = vim.api.nvim_win_get_cursor(0)[1]
    M.activate()
  end
end

function M.setup(opts)
  M.cfg = vim.tbl_extend("force", M.cfg, opts or {})
  local group = vim.api.nvim_create_augroup("Centered", { clear = true })
  vim.api.nvim_create_autocmd("CursorMovedI", {
    group = group,
    callback = function()
      center(true)
    end,
  })
  vim.api.nvim_create_autocmd({ "CursorMoved", "BufEnter" }, {
    group = group,
    callback = function()
      center(false)
    end,
  })
  vim.on_key(M.cfg.disable_on_mouse and on_mouse or nil, vim.api.nvim_create_namespace("centered"))
end

function M.enable()
  M.cfg.enabled = true
  center(false)
end

function M.disable()
  M.cfg.enabled = false
end

function M.toggle()
  if M.cfg.enabled then
    M.disable()
  else
    M.enable()
  end
end

function M.activate()
  M.cfg.active = true
end

function M.deactivate()
  M.cfg.active = false
end

return M
