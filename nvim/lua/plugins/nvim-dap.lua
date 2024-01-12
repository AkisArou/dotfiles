return {
  "mfussenegger/nvim-dap",
  event = "VimEnter",
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

    dap.configurations["typescript"] = {
      {
        type      = "pwa-node",
        request   = "attach",
        name      = "Volunteer: Attach Volunteer NodeJS App",
        port      = 9228,
        skipFiles = { "<node_internals>/**" }
      }
    }
  end
}
