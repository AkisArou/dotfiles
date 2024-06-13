-- return {
--   {
--     "telescope.nvim",
--     keys = {
--       { "<leader>ff", Util.telescope("files", { cwd = false }), desc = "Find Files (root dir)" },
--       { "<leader>fF", Util.telescope("files"), desc = "Find Files (cwd)" },
--     },
--     opts = {
--       pickers = {
--         git_files = file_pickers_config,
--         find_files = file_pickers_config,
--       },
--     },
--     extensions = {
--       coc = {
--         theme = "ivy",
--         prefer_locations = true, -- always use Telescope locations to preview definitions/declarations/implementations etc
--       },
--     },
--     dependencies = {
--       {
--         "nvim-telescope/telescope-fzf-native.nvim",
--         build = "make",
--         config = function()
--           require("telescope").load_extension("fzf")
--         end,
--       },
--       -- {
--       --   "fannheyward/telescope-coc.nvim",
--       --   config = function()
--       --     require('telescope').load_extension('coc')
--       --   end,
--       -- }
--     },
--   },
-- }

return {
  "nvim-telescope/telescope.nvim",
  version = false,
  dependencies = {
    "nvim-lua/plenary.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      enabled = vim.fn.executable("make") == 1,
      config = function()
        require("telescope").load_extension("fzf")
      end,
    },
    "nvim-tree/nvim-web-devicons",
    -- {
    --   "fannheyward/telescope-coc.nvim",
    --   config = function()
    --     require('telescope').load_extension('coc')
    --   end,
    -- }
  },
  config = function()
    local telescope = require("telescope")
    local actions = require("telescope.actions")

    telescope.setup({
      pickers = {
        find_files = {
          sorting_strategy = "ascending",
          -- layout_strategy = "flex",
          layout_config = {
            -- height = 0.95,
            prompt_position = "top",
            -- preview_height = 0,
            preview_width = 0.25,
          },
        },
      },

      defaults = {
        path_display = { "truncate " },
        mappings = {
          i = {
            ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
          },
        },
      },
    })

    telescope.load_extension("fzf")

    -- set keymaps
    local keymap = vim.keymap -- for conciseness

    keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Fuzzy find files in cwd" })
    keymap.set("n", "<leader>fr", "<cmd>Telescope oldfiles<cr>", { desc = "Fuzzy find recent files" })
    keymap.set("n", "<leader>fs", "<cmd>Telescope live_grep<cr>", { desc = "Find string in cwd" })
    keymap.set("n", "<leader>fc", "<cmd>Telescope grep_string<cr>", { desc = "Find string under cursor in cwd" })

    keymap.set("n", "<leader>fd", "<cmd>Telescope diagnostics bufnr=0<cr>", { desc = "Document diagnostics" })
    keymap.set("n", "<leader>sD", "<cmd>Telescope diagnostics<cr>", { desc = "Workspace diagnostics" })
  end,

  opts = function()
    local actions = require("telescope.actions")

    local open_with_trouble = function(...)
      return require("trouble.providers.telescope").open_with_trouble(...)
    end
    local open_selected_with_trouble = function(...)
      return require("trouble.providers.telescope").open_selected_with_trouble(...)
    end

    return {
      defaults = {
        prompt_prefix = " ",
        selection_caret = " ",
        -- open files in the first window that is an actual file.
        -- use the current window if no other window is available.
        get_selection_window = function()
          local wins = vim.api.nvim_list_wins()
          table.insert(wins, 1, vim.api.nvim_get_current_win())
          for _, win in ipairs(wins) do
            local buf = vim.api.nvim_win_get_buf(win)
            if vim.bo[buf].buftype == "" then
              return win
            end
          end
          return 0
        end,
        mappings = {
          i = {
            ["<c-t>"] = open_with_trouble,
            ["<a-t>"] = open_selected_with_trouble,
            ["<C-Down>"] = actions.cycle_history_next,
            ["<C-Up>"] = actions.cycle_history_prev,
            ["<C-f>"] = actions.preview_scrolling_down,
            ["<C-b>"] = actions.preview_scrolling_up,
          },
          n = {
            ["q"] = actions.close,
          },
        },
      },
    }
  end,

  -- extensions = {
  --   coc = {
  --     theme = "ivy",
  --     prefer_locations = true, -- always use Telescope locations to preview definitions/declarations/implementations etc
  --   },
  -- },
}

-- return {
--   "nvim-telescope/telescope.nvim",
--   cmd = "Telescope",
--   version = false, -- telescope did only one release, so use HEAD for now
--   dependencies = {
--   },
--   keys = {
--     {
--       "<leader>,",
--       "<cmd>Telescope buffers sort_mru=true sort_lastused=true<cr>",
--       desc = "Switch Buffer",
--     },
--     { "<leader>/", Util.telescope("live_grep"), desc = "Grep (root dir)" },
--     { "<leader>:", "<cmd>Telescope command_history<cr>", desc = "Command History" },
--     { "<leader><space>", Util.telescope("files"), desc = "Find Files (root dir)" },
--     -- find
--     { "<leader>fb", "<cmd>Telescope buffers sort_mru=true sort_lastused=true<cr>", desc = "Buffers" },
--     { "<leader>fc", Util.telescope.config_files(), desc = "Find Config File" },
--     { "<leader>ff", Util.telescope("files", { cwd = false }), desc = "Find Files (root dir)" },
--     { "<leader>fF", Util.telescope("files"), desc = "Find Files (cwd)" },
--     { "<leader>fg", "<cmd>Telescope git_files<cr>", desc = "Find Files (git-files)" },
--     { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent" },
--     { "<leader>fR", Util.telescope("oldfiles", { cwd = vim.loop.cwd() }), desc = "Recent (cwd)" },
--     -- git
--     { "<leader>gc", "<cmd>Telescope git_commits<CR>", desc = "commits" },
--     { "<leader>gs", "<cmd>Telescope git_status<CR>", desc = "status" },
--     -- search
--     { '<leader>s"', "<cmd>Telescope registers<cr>", desc = "Registers" },
--     { "<leader>sa", "<cmd>Telescope autocommands<cr>", desc = "Auto Commands" },
--     { "<leader>sb", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Buffer" },
--     { "<leader>sc", "<cmd>Telescope command_history<cr>", desc = "Command History" },
--     { "<leader>sC", "<cmd>Telescope commands<cr>", desc = "Commands" },
--     { "<leader>sg", Util.telescope("live_grep"), desc = "Grep (root dir)" },
--     { "<leader>sG", Util.telescope("live_grep", { cwd = false }), desc = "Grep (cwd)" },
--     { "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "Help Pages" },
--     { "<leader>sH", "<cmd>Telescope highlights<cr>", desc = "Search Highlight Groups" },
--     { "<leader>sk", "<cmd>Telescope keymaps<cr>", desc = "Key Maps" },
--     { "<leader>sM", "<cmd>Telescope man_pages<cr>", desc = "Man Pages" },
--     { "<leader>sm", "<cmd>Telescope marks<cr>", desc = "Jump to Mark" },
--     { "<leader>so", "<cmd>Telescope vim_options<cr>", desc = "Options" },
--     { "<leader>sR", "<cmd>Telescope resume<cr>", desc = "Resume" },
--     { "<leader>sw", Util.telescope("grep_string", { word_match = "-w" }), desc = "Word (root dir)" },
--     { "<leader>sW", Util.telescope("grep_string", { cwd = false, word_match = "-w" }), desc = "Word (cwd)" },
--     { "<leader>sw", Util.telescope("grep_string"), mode = "v", desc = "Selection (root dir)" },
--     { "<leader>sW", Util.telescope("grep_string", { cwd = false }), mode = "v", desc = "Selection (cwd)" },
--     { "<leader>uC", Util.telescope("colorscheme", { enable_preview = true }), desc = "Colorscheme with preview" },
--     {
--       "<leader>ss",
--       function()
--         require("telescope.builtin").lsp_document_symbols({
--           symbols = require("lazyvim.config").get_kind_filter(),
--         })
--       end,
--       desc = "Goto Symbol",
--     },
--     {
--       "<leader>sS",
--       function()
--         require("telescope.builtin").lsp_dynamic_workspace_symbols({
--           symbols = require("lazyvim.config").get_kind_filter(),
--         })
--       end,
--       desc = "Goto Symbol (Workspace)",
--     },
--   },
--   opts = function()
--     local actions = require("telescope.actions")
--
--     local open_with_trouble = function(...)
--       return require("trouble.providers.telescope").open_with_trouble(...)
--     end
--     local open_selected_with_trouble = function(...)
--       return require("trouble.providers.telescope").open_selected_with_trouble(...)
--     end
--     local find_files_no_ignore = function()
--       local action_state = require("telescope.actions.state")
--       local line = action_state.get_current_line()
--       Util.telescope("find_files", { no_ignore = true, default_text = line })()
--     end
--     local find_files_with_hidden = function()
--       local action_state = require("telescope.actions.state")
--       local line = action_state.get_current_line()
--       Util.telescope("find_files", { hidden = true, default_text = line })()
--     end
--
--     return {
--       defaults = {
--         prompt_prefix = " ",
--         selection_caret = " ",
--         -- open files in the first window that is an actual file.
--         -- use the current window if no other window is available.
--         get_selection_window = function()
--           local wins = vim.api.nvim_list_wins()
--           table.insert(wins, 1, vim.api.nvim_get_current_win())
--           for _, win in ipairs(wins) do
--             local buf = vim.api.nvim_win_get_buf(win)
--             if vim.bo[buf].buftype == "" then
--               return win
--             end
--           end
--           return 0
--         end,
--         mappings = {
--           i = {
--             ["<c-t>"] = open_with_trouble,
--             ["<a-t>"] = open_selected_with_trouble,
--             ["<a-i>"] = find_files_no_ignore,
--             ["<a-h>"] = find_files_with_hidden,
--             ["<C-Down>"] = actions.cycle_history_next,
--             ["<C-Up>"] = actions.cycle_history_prev,
--             ["<C-f>"] = actions.preview_scrolling_down,
--             ["<C-b>"] = actions.preview_scrolling_up,
--           },
--           n = {
--             ["q"] = actions.close,
--           },
--         },
--       },
--     }
--   end,
-- }
