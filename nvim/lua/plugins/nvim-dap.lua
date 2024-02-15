return {
  {
    "jay-babu/mason-nvim-dap.nvim",
    enabled = false,
    opts = {
      automatic_installation = true,
      ensure_installed = {
        "js",
        "chrome"
      }
    }
  },
  {
    "mfussenegger/nvim-dap",
    event = "VeryLazy",
    enabled = false,
    opts = function()
      local dap = require("dap")
      if not dap.adapters["pwa-node"] then
        require("dap").adapters["pwa-node"] = {
          type = "server",
          host = "localhost",
          port = "${port}",
          executable = {
            command = "node",
            -- 💀 Make sure to update this path to point to your installation
            args = {
              require("mason-registry").get_package("js-debug-adapter"):get_install_path()
              .. "/js-debug/src/dapDebugServer.js",
              "${port}",
            },
          },
        }
      end

      if not dap.adapters.chrome then
        dap.adapters.chrome = {
          type = "executable",
          command = "node",
          args = {
            require("mason-registry").get_package("chrome-debug-adapter"):get_install_path()
            .. "/out/src/chromeDebug.js" }
        }
      end

      dap.configurations["typescript"] = {
        {
          type      = "pwa-node",
          request   = "attach",
          name      = "Volunteer: Attach Volunteer NodeJS App",
          port      = 9228,
          skipFiles = { "<node_internals>/**" }
        }
      }

      dap.configurations["typescriptreact"] = {
        {
          type = "chrome",
          request = "launch",
          name = "PRM: Launch Back Office",
          runtimeExecutable = "/usr/bin/brave",
          url = "https://localhost:5173",
          webRoot = "${workspaceFolder}/apps/client/assistant-prm-airport/back-office"
        },
        {
          type = "chrome",
          request = "attach",
          name = "PRM: Attach Back Office",
          -- "urlFilter= "https://localhost:5173/*",
          webRoot = "${workspaceFolder}/apps/client/assistant-prm-airport/back-office",
          port = 9229
        },
        {
          type = "chrome",
          request = "launch",
          name = "Volunteer: Launch Back Office",
          runtimeExecutable = "/usr/bin/brave",
          url = "http://localhost:4200",
          webRoot = "${workspaceFolder}/apps/client/assistant-prm-airport/back-office"
        },
        {
          type = "chrome",
          request = "attach",
          name = "Volunteer: Attach Back Office",
          -- "urlFilter= "https://localhost:5173/*",
          webRoot = "${workspaceFolder}/apps/client/assistant-prm-airport/back-office",
          port = 9229
        }
      }
    end
  }
}
