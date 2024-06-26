return {
  "nvim-tree/nvim-tree.lua",
  dependencies = {
    {
      "antosha417/nvim-lsp-file-operations",
      dependencies = {
        "nvim-lua/plenary.nvim",
      },
      config = function()
        require("lsp-file-operations").setup()
      end,
    },
  },
  config = function()
    local api = require("nvim-tree.api")

    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1

    vim.api.nvim_set_keymap("n", "<leader>e", ":NvimTreeToggle<cr>", { silent = true, noremap = true })

    -- Clear clipboard before every copy & cut operation
    local function clear_and_copy()
      api.fs.clear_clipboard()
      api.fs.copy.node()
    end

    local function clear_and_cut()
      api.fs.clear_clipboard()
      api.fs.cut()
    end

    -- Make :bd and :q behave as usual when tree is visible
    vim.api.nvim_create_autocmd({ "BufEnter", "QuitPre" }, {
      nested = false,
      callback = function(e)
        local tree = require("nvim-tree.api").tree

        if not tree.is_visible() then
          return
        end

        local winCount = 0
        for _, winId in ipairs(vim.api.nvim_list_wins()) do
          if vim.api.nvim_win_get_config(winId).focusable then
            winCount = winCount + 1
          end
        end

        if e.event == "QuitPre" and winCount == 2 then
          vim.api.nvim_cmd({ cmd = "qall" }, {})
        end

        if e.event == "BufEnter" and winCount == 1 then
          vim.defer_fn(function()
            tree.toggle({ find_file = true, focus = true })
            tree.toggle({ find_file = true, focus = false })
          end, 10)
        end
      end,
    })

    -- Automatically open file upon creation
    api.events.subscribe(api.events.Event.FileCreated, function(file)
      vim.cmd("edit " .. file.fname)
    end)

    -- h, j, k, l Style Navigation And Editing
    local function edit_or_open()
      api.node.open.edit()
    end

    local function vsplit_preview()
      local node = api.tree.get_node_under_cursor()

      if node.nodes ~= nil then
        api.node.open.edit()
      else
        api.node.open.vertical()
      end

      api.tree.focus()
    end

    -- Toggle Adaptive Width
    local VIEW_WIDTH_FIXED = 30
    local view_width_max = VIEW_WIDTH_FIXED -- fixed to start

    local function toggle_width_adaptive()
      if view_width_max == -1 then
        view_width_max = VIEW_WIDTH_FIXED
      else
        view_width_max = -1
      end

      require("nvim-tree.api").tree.reload()
    end

    local function get_view_width_max()
      return view_width_max
    end

    -- On attach
    local function on_attach(bufnr)
      local function opts(desc)
        return { desc = "nvim-tree: " .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
      end

      api.config.mappings.default_on_attach(bufnr)

      vim.keymap.set("n", "l", edit_or_open, opts("Edit Or Open"))
      vim.keymap.set("n", "L", vsplit_preview, opts("Vsplit Preview"))
      vim.keymap.set("n", "h", api.node.navigate.parent_close, opts("Close parent"))
      vim.keymap.set("n", "H", api.tree.toggle_hidden_filter, opts("Toggle hidden"))
      vim.keymap.set("n", "W", api.tree.collapse_all, opts("Collapse All"))
      vim.keymap.set("n", "c", clear_and_copy, opts("Copy"))
      vim.keymap.set("n", "x", clear_and_cut, opts("Cut"))
      vim.keymap.set("n", "e", toggle_width_adaptive, opts("Toggle Adaptive Width"))
    end

    require("nvim-tree").setup({
      on_attach = on_attach,
      update_focused_file = {
        enable = true,
        update_root = {
          enable = false,
        },
      },
      diagnostics = {
        enable = true,
        show_on_dirs = true,
      },
      view = {
        width = {
          min = 30,
          max = get_view_width_max,
        },
      },
      filters = {
        dotfiles = true,
      },
    })
  end,
}
