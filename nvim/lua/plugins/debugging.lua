return {
  {
    "mfussenegger/nvim-dap",
    -- event = "VeryLazy",
    dependencies = {
      { "rcarriga/nvim-dap-ui" },
      { "nvim-neotest/nvim-nio" },
      {
        "theHamsta/nvim-dap-virtual-text",
        opts = {},
      },
    },
    keys = {
      -- dap
      {
        "<leader>dB",
        function()
          require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
        end,
        desc = "Breakpoint Condition",
      },
      {
        "<leader>db",
        function()
          require("dap").toggle_breakpoint()
        end,
        desc = "Toggle Breakpoint",
      },
      {
        "<leader>dc",
        function()
          require("dap").continue()
        end,
        desc = "Continue",
      },
      {
        "<leader>da",
        function()
          require("dap").continue({ before = get_args })
        end,
        desc = "Run with Args",
      },
      {
        "<leader>dC",
        function()
          require("dap").run_to_cursor()
        end,
        desc = "Run to Cursor",
      },
      {
        "<leader>dg",
        function()
          require("dap").goto_()
        end,
        desc = "Go to line (no execute)",
      },
      {
        "<leader>di",
        function()
          require("dap").step_into()
        end,
        desc = "Step Into",
      },
      {
        "<leader>dj",
        function()
          require("dap").down()
        end,
        desc = "Down",
      },
      {
        "<leader>dk",
        function()
          require("dap").up()
        end,
        desc = "Up",
      },
      {
        "<leader>dl",
        function()
          require("dap").run_last()
        end,
        desc = "Run Last",
      },
      {
        "<leader>do",
        function()
          require("dap").step_out()
        end,
        desc = "Step Out",
      },
      {
        "<leader>dO",
        function()
          require("dap").step_over()
        end,
        desc = "Step Over",
      },
      {
        "<leader>dp",
        function()
          require("dap").pause()
        end,
        desc = "Pause",
      },
      {
        "<leader>dr",
        function()
          require("dap").repl.toggle()
        end,
        desc = "Toggle REPL",
      },
      {
        "<leader>ds",
        function()
          require("dap").session()
        end,
        desc = "Session",
      },
      {
        "<leader>dt",
        function()
          require("dap").terminate()
        end,
        desc = "Terminate",
      },
      {
        "<leader>dw",
        function()
          require("dap.ui.widgets").hover()
        end,
        desc = "Widgets",
      },

      -- dapui
      {
        "<leader>du",
        function()
          require("dapui").toggle({})
        end,
        desc = "Dap UI",
      },
      {
        "<leader>de",
        function()
          require("dapui").eval()
        end,
        desc = "Eval",
        mode = { "n", "v" },
      },
    },
    config = function()
      local dap = require("dap")
      local dapui = require("dapui")

      dapui.setup()

      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open({})
      end
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close({})
      end
      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close({})
      end

      dap.adapters.firefox = {
        type = "executable",
        command = "node",
        args = {
          require("mason-registry").get_package("firefox-debug-adapter"):get_install_path()
            .. "/dist/adapter.bundle.js",
          "${port}",
        },
      }

      dap.configurations.typescriptreact = {
        {
          name = "Debug with Firefox",
          type = "firefox",
          request = "launch",
          reAttach = true,
          url = "http://localhost:5173",
          webRoot = "${workspaceFolder}",
          firefoxExecutable = "/usr/bin/firefox",
        },
      }

      dap.adapters["pwa-node"] = {
        type = "server",
        host = "localhost",
        port = "${port}",
        executable = {
          command = "node",
          args = {
            require("mason-registry").get_package("js-debug-adapter"):get_install_path()
              .. "/js-debug/src/dapDebugServer.js",
            "${port}",
          },
        },
      }

      dap.configurations["typescript"] = {
        {
          type = "pwa-node",
          request = "attach",
          name = "Volunteer: Attach Volunteer NodeJS App",
          port = 9228,
          skipFiles = { "<node_internals>/**" },
        },
      }

      dap.configurations["typescriptreact"] = {
        {
          type = "chrome",
          request = "launch",
          name = "PRM: Launch Back Office",
          runtimeExecutable = "/usr/bin/brave",
          url = "https://localhost:5173",
          webRoot = "${workspaceFolder}/apps/client/assistant-prm-airport/back-office",
        },
        {
          type = "chrome",
          request = "attach",
          name = "PRM: Attach Back Office",
          -- "urlFilter= "https://localhost:5173/*",
          webRoot = "${workspaceFolder}/apps/client/assistant-prm-airport/back-office",
          port = 9229,
        },
        {
          type = "chrome",
          request = "launch",
          name = "Volunteer: Launch Back Office",
          runtimeExecutable = "/usr/bin/brave",
          url = "http://localhost:4200",
          webRoot = "${workspaceFolder}/apps/client/assistant-prm-airport/back-office",
        },
        {
          type = "chrome",
          request = "attach",
          name = "Volunteer: Attach Back Office",
          -- "urlFilter= "https://localhost:5173/*",
          webRoot = "${workspaceFolder}/apps/client/assistant-prm-airport/back-office",
          port = 9229,
        },
      }
    end,
  },
}
