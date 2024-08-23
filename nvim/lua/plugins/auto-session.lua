function Close_empty_unnamed_buffers()
  local buffers = vim.api.nvim_list_bufs()

  for _, bufnr in ipairs(buffers) do
    if vim.api.nvim_buf_is_loaded(bufnr) and vim.api.nvim_buf_get_name(bufnr) == "" then
      vim.api.nvim_buf_delete(bufnr, {
        force = true,
      })
    end
  end
end

return {
  "rmagatti/auto-session",
  lazy = false,

  config = function()
    vim.o.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions"

    -- Register the autocommand after Vim has entered

    require("auto-session").setup({
      auto_session_suppress_dirs = { "~/", "~/Downloads", "/" },
      post_restore_cmds = {
        -- Initiated here because has problems with auto-session
        ':lua vim.api.nvim_command("autocmd BufReadPost * lua Close_empty_unnamed_buffers()")',
      },
      -- log_level = "debug",
    })
  end,
}
