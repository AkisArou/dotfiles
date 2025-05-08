return {
  {
    "mfussenegger/nvim-dap",
    event = "VeryLazy",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "nvim-neotest/nvim-nio",
      "theHamsta/nvim-dap-virtual-text",
      "nvim-lua/plenary.nvim",
      {
        "microsoft/vscode-js-debug",
        build = "npm install --legacy-peer-deps && npx gulp dapDebugServer",
      },
    },
    config = function()
      local dap = require("dap")
      local dapui = require("dapui")

      dapui.setup()
      -- stylua: ignore
      dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open({}) end
      -- stylua: ignore
      dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close({}) end
      -- stylua: ignore
      dap.listeners.before.event_exited["dapui_config"] = function() dapui.close({}) end

      vim.fn.sign_define("DapBreakpoint", { text = "🛑", texthl = "", linehl = "", numhl = "" })

      local chrome_type = "pwa-chrome"
      local node_type = "node"

      require("dap.ext.vscode").json_decode = function(str)
        local config = vim.json.decode(require("plenary.json").json_strip_comments(str))

        for _, item in ipairs(config.configurations) do
          if item.type == "chrome" then
            item.type = chrome_type
          end
        end

        return config
      end

      local js_debug_adapter_opts = {
        type = "server",
        host = "localhost",
        port = "${port}",
        executable = {
          command = "node",
          -- stylua: ignore
          args = {
            vim.fn.stdpath("data") .. "/lazy/vscode-js-debug/dist/src/dapDebugServer.js", "${port}",
          },
        },
      }

      dap.adapters[node_type] = js_debug_adapter_opts
      dap.adapters[chrome_type] = js_debug_adapter_opts

      if not vim.g.is_work then
        dap.configurations.typescriptreact = {
          -- Default react
          {
            type = chrome_type,
            name = "Default react",
            request = "attach",
            program = "${file}",
            cwd = vim.fn.getcwd(),
            sourceMaps = true,
            protocol = "inspector",
            port = 9222,
            webRoot = "${workspaceFolder}",
          },
        }

        dap.configurations.typescript = {
          -- Default node
          {
            type = node_type,
            request = "Default node",
            name = "Launch file",
            runtimeExecutable = "node",
            -- runtimeArgs = {
            --   "run",
            --   "--inspect-wait",
            --   "--allow-all",
            -- },
            program = "${file}",
            cwd = "${workspaceFolder}",
            attachSimplePort = 9229,
          },
        }
      end
    end,
    -- stylua: ignore
    keys = {
      { "<leader>dB", function() require("dap").set_breakpoint(vim.fn.input('Breakpoint condition: ')) end, desc = "Breakpoint Condition" },
      { "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Toggle Breakpoint" },
      { "<leader>dc", function() require("dap").continue() end, desc = "Run/Continue" },
      -- { "<leader>da", function() require("dap").continue({ before = get_args }) end, desc = "Run with Args" },
      { "<leader>dC", function() require("dap").run_to_cursor() end, desc = "Run to Cursor" },
      { "<leader>dg", function() require("dap").goto_() end, desc = "Go to Line (No Execute)" },
      { "<leader>di", function() require("dap").step_into() end, desc = "Step Into" },
      { "<leader>dj", function() require("dap").down() end, desc = "Down" },
      { "<leader>dk", function() require("dap").up() end, desc = "Up" },
      { "<leader>dl", function() require("dap").run_last() end, desc = "Run Last" },
      { "<leader>do", function() require("dap").step_out() end, desc = "Step Out" },
      { "<leader>dO", function() require("dap").step_over() end, desc = "Step Over" },
      { "<leader>dP", function() require("dap").pause() end, desc = "Pause" },
      { "<leader>dr", function() require("dap").repl.toggle() end, desc = "Toggle REPL" },
      { "<leader>ds", function() require("dap").session() end, desc = "Session" },
      { "<leader>dt", function() require("dap").terminate() end, desc = "Terminate" },
      { "<leader>dw", function() require("dap.ui.widgets").hover() end, desc = "Widgets" },
    },
  },
}
