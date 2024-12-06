local M = {}

function CloseBuffers(filter)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    local shouldClose = filter(buf)
    if shouldClose then
      local buf_name = vim.api.nvim_buf_get_name(buf)

      if vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_buf_is_loaded(buf) then
        if not buf_name:match("NeogitStatus") then
          if vim.api.nvim_get_option_value("modified", { buf = buf }) then
            vim.api.nvim_command("bufdo silent! w")
          end

          vim.api.nvim_buf_delete(buf, { force = true })
        end
      end
    end
  end
end

function M.CloseOtherBuffers()
  local current_buf = vim.api.nvim_get_current_buf()

  CloseBuffers(function(buf)
    return buf ~= current_buf
  end)
end

function M.CloseAllBuffers()
  CloseBuffers(function()
    return true
  end)
end

function M.CloseCurrentBuffer()
  local current_buf = vim.api.nvim_get_current_buf()

  CloseBuffers(function(buf)
    return buf == current_buf
  end)
end

return M
