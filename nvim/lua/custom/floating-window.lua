local M = {}

function M.create_floating_window_api(cb)
  local state = {
    floating = {
      buf = -1,
      win = -1,
    },
  }

  local function create_floating_window(opts)
    opts = opts or {}
    local width = opts.width or math.floor(vim.o.columns * 0.9)
    local height = opts.height or math.floor(vim.o.lines * 0.9)

    -- Calculate the position to center the window
    local col = math.floor((vim.o.columns - width) / 2)
    local row = math.floor((vim.o.lines - height) / 2)

    -- Create a buffer
    local buf = nil
    if vim.api.nvim_buf_is_valid(opts.buf) then
      buf = opts.buf
    else
      buf = vim.api.nvim_create_buf(false, true) -- No file, scratch buffer
    end

    -- Define window configuration
    local win_config = {
      relative = "editor",
      width = width,
      height = height,
      col = col,
      row = row,
      style = "minimal", -- No borders or extra UI elements
      border = "rounded",
    }

    -- Create the floating window
    local win = vim.api.nvim_open_win(buf, true, win_config)

    return { buf = buf, win = win }
  end

  local toggle = function()
    if not vim.api.nvim_win_is_valid(state.floating.win) then
      state.floating = create_floating_window({ buf = state.floating.buf })
      cb(state)
    else
      vim.api.nvim_win_hide(state.floating.win)
    end
  end

  local open = function()
    if not vim.api.nvim_win_is_valid(state.floating.win) then
      state.floating = create_floating_window({ buf = state.floating.buf })
      cb(state)
    end
  end

  local close = function()
    if not vim.api.nvim_win_is_valid(state.floating.win) then
      state.floating = create_floating_window({ buf = state.floating.buf })
    else
      vim.api.nvim_win_close(state.floating.win, true)
      vim.api.nvim_buf_delete(state.floating.buf, { force = true })
    end
  end

  local toggle_fresh = function()
    if not vim.api.nvim_win_is_valid(state.floating.win) then
      open()
    else
      close()
    end
  end

  return {
    toggle = toggle,
    open = open,
    close = close,
    toggle_fresh = toggle_fresh,
  }
end

return M
