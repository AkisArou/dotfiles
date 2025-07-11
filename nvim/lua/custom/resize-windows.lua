local M = {}

local window_cache = {
  sorted = nil,
  valid = false,
}

vim.api.nvim_create_autocmd({ "WinNew", "WinClosed", "WinEnter" }, {
  callback = function()
    window_cache.valid = false
  end,
})

local function get_windows_sorted_by_left()
  if window_cache.valid and window_cache.sorted then
    return window_cache.sorted
  end

  local windows = vim.api.nvim_list_wins()
  local win_positions = {}

  for _, win in ipairs(windows) do
    local pos = vim.api.nvim_win_get_position(win)
    table.insert(win_positions, { win = win, col = pos[2] })
  end

  table.sort(win_positions, function(a, b)
    return a.col < b.col
  end)

  local sorted_windows = {}
  for _, item in ipairs(win_positions) do
    table.insert(sorted_windows, item.win)
  end

  -- Cache it
  window_cache.sorted = sorted_windows
  window_cache.valid = true

  return sorted_windows
end

local function get_left_neighbor(sorted_wins)
  local current_win = vim.api.nvim_get_current_win()

  for i, win in ipairs(sorted_wins) do
    if win == current_win and i > 1 then
      return sorted_wins[i - 1]
    end
  end
  return nil -- No left neighbor (current is the leftmost)
end

local function resize_window(delta)
  local wins = get_windows_sorted_by_left()
  local cur_win = vim.api.nvim_get_current_win()

  if #wins == 0 then
    return
  end

  if #wins >= 3 and cur_win ~= wins[1] and cur_win ~= wins[#wins] then
    local prev_win = get_left_neighbor(wins)

    if prev_win then
      vim.api.nvim_win_set_width(prev_win, vim.api.nvim_win_get_width(prev_win) + delta)
    end
  else
    if cur_win ~= wins[#wins] then
      vim.cmd(string.format("vertical resize %s%d", delta > 0 and "+" or "-", math.abs(delta)))
    else
      vim.cmd(string.format("vertical resize %s%d", delta < 0 and "+" or "-", math.abs(delta)))
    end
  end
end

M.resize_window_left = function()
  resize_window(-2)
end

M.resize_window_right = function()
  resize_window(2)
end

return M
