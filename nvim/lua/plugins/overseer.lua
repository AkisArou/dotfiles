return {
  "stevearc/overseer.nvim",
  dependencies = { "folke/trouble.nvim" },
  config = function()
    local problem_matcher = {
      fileLocation = { "relative", "${cwd}" },
      pattern = {
        regexp = "^([^\\s].*)[\\(:](\\d+)[,:](\\d+)(?:\\):\\s+|\\s+-\\s+)(error|warning|info)\\s+TS(\\d+)\\s*:\\s*(.*)$",
        vim_regexp = "\\v^([^[:space:]].*)[\\(:](\\d+)[,:](\\d+)%(\\):\\s+|\\s+-\\s+)(error|warning|info)\\s+TS(\\d+)\\s*:\\s*(.*)$",
        file = 1,
        line = 2,
        column = 3,
        severity = 4,
        code = 5,
        message = 6,
      },
      owner = "typescript",
      source = "ts",
      applyTo = "closedDocuments",
      background = {
        activeOnStart = true,
        beginsPattern = {
          regexp = "^\\s*(?:message TS6032:|\\[?\\D*.{1,2}[:.].{1,2}[:.].{1,2}\\D*(├\\D*\\d{1,2}\\D+┤)?(?:\\]| -)) (Starting compilation in watch mode|File change detected\\. Starting incremental compilation)\\.\\.\\.",
          lua_pat = "(Starting compilation in watch mode|File change detected%. Starting incremental compilation)%.%.%.$",
        },
        endsPattern = {
          regexp = "^\\s*(?:message TS6042:|\\[?\\D*.{1,2}[:.].{1,2}[:.].{1,2}\\D*(├\\D*\\d{1,2}\\D+┤)?(?:\\]| -)) (?:Compilation complete\\.|Found \\d+ errors?\\.) Watching for file changes\\.",
          lua_pat = "Watching for file changes%.$",
        },
      },
    }

    local overseer = require("overseer")

    overseer.setup({
      strategy = {
        "toggleterm",
        open_on_start = false,
      },
    })

    local cwd = vim.fn.getcwd()
    local template_name = "nable-tsc"

    local is_nable = cwd:match("nable%-solutions")

    overseer.register_template({
      name = template_name,
      condition = {
        callback = function()
          return is_nable
        end,
      },
      builder = function()
        return {
          cmd = { "pnpm", "ts:watch" },
          env = {
            NO_COLOR = "1",
          },
          components = {
            { "on_output_parse", problem_matcher = problem_matcher },
            { "on_result_notify" },
            { "on_result_diagnostics_quickfix" },
            "default",
          },
        }
      end,
    })

    if is_nable then
      vim.api.nvim_create_autocmd({ "TermEnter" }, {
        callback = function()
          for _, buffers in ipairs(vim.fn.getbufinfo()) do
            vim.api.nvim_create_autocmd({ "BufWriteCmd", "FileWriteCmd", "FileAppendCmd" }, {
              buffer = buffers.bufnr,
              callback = function()
                vim.cmd("silent q!")
              end,
            })
          end
        end,
      })

      overseer.run_template({ name = template_name, autostart = true }, function(task)
        if task == nil then
          print("NIL TASK")
          return
        end

        print("OK TASK")
      end)
    end
  end,
}
