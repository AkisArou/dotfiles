local M = {}

function M.install()
  local cur_win = vim.api.nvim_get_current_win() -- remember original window

  -- open terminal split
  vim.cmd.vnew()
  local term_buf = vim.api.nvim_get_current_buf()
  vim.cmd("terminal pnpm install")
  vim.cmd.wincmd("L")
  vim.api.nvim_win_set_width(0, 40) -- set desired width

  -- leave insert mode so Neovim doesn't stay in it
  vim.cmd("stopinsert")

  -- return focus to original window
  vim.api.nvim_set_current_win(cur_win)

  -- auto-close terminal when job exits with code 0
  vim.api.nvim_create_autocmd("TermClose", {
    buffer = term_buf,
    callback = function()
      local status = vim.v.event.status
      if status == 0 then
        vim.schedule(function()
          -- close the terminal window
          pcall(vim.api.nvim_buf_delete, term_buf, { force = true })
        end)
      end
    end,
  })
end

return M
