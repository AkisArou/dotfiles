local M = {
  delta = 2,
}

local resize_cmds = {
  shrink = "",
  grow = "",
}

local function build_resize_cmds(delta)
  resize_cmds.shrink = "vertical resize -" .. delta
  resize_cmds.grow = "vertical resize +" .. delta
end

local function get_windows_sorted_by_left()
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

local function resize_window(delta_middle, resize_left_cmd, resize_right_cmd)
  local wins = get_windows_sorted_by_left()
  local cur_win = vim.api.nvim_get_current_win()

  if #wins == 0 then
    return
  end

  if #wins >= 3 and cur_win ~= wins[1] and cur_win ~= wins[#wins] then
    local prev_win = get_left_neighbor(wins)

    if prev_win then
      vim.api.nvim_win_set_width(prev_win, vim.api.nvim_win_get_width(prev_win) + delta_middle)
    end
  else
    if cur_win ~= wins[#wins] then
      vim.cmd(resize_left_cmd)
    else
      vim.cmd(resize_right_cmd)
    end
  end
end

M.setup = function(config)
  M.delta = config.delta
  build_resize_cmds(config.delta)
end

M.resize_window_left = function()
  resize_window(-M.delta, resize_cmds.shrink, resize_cmds.grow)
end

M.resize_window_right = function()
  resize_window(M.delta, resize_cmds.grow, resize_cmds.shrink)
end

return M
