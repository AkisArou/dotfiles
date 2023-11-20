local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values

function PickTargetWorkspace(workspaces)
  local opts = require("telescope.themes").get_dropdown({})
  local sorter = conf.generic_sorter(opts)

  pickers
    .new(opts, {
      prompt_title = "Target workspace",
      finder = finders.new_table({
        results = workspaces,
      }),
      sorter = sorter,
      attach_mappings = function(prompt_bufnr, map)
        actions.select_default:replace(function()
          actions.close(prompt_bufnr)
          local selection = action_state.get_selected_entry()
          PickDepWorkspace(workspaces, selection[1])
        end)
        return true
      end,
    })
    :find()
end

function PickDepWorkspace(workspaces, target)
  local opts = require("telescope.themes").get_dropdown({})
  local sorter = conf.generic_sorter(opts)

  pickers
    .new(opts, {
      prompt_title = "Dependency workspace",
      finder = finders.new_table({
        results = workspaces,
      }),
      sorter = sorter,
      attach_mappings = function(prompt_bufnr, map)
        actions.select_default:replace(function()
          actions.close(prompt_bufnr)
          local selection = action_state.get_selected_entry()
          local dep = selection[1]
          local args = target .. "," .. dep
          vim.cmd("GotSelections" .. args)
        end)
        return true
      end,
    })
    :find()
end

vim.keymap.set("n", "<leader>l", ":GetPnpmWorkspaces<CR>")
