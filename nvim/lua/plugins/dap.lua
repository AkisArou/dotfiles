require("mason-nvim-dap").setup({
  automatic_installation = true,
  ensure_installed = { "js" },
})
local dap = require("dap")
local dapui = require("dapui")

dapui.setup()

dap.listeners.after.event_initialized["dapui_config"] = dapui.open
dap.listeners.before.event_terminated["dapui_config"] = dapui.close
dap.listeners.before.event_exited["dapui_config"] = dapui.close

vim.api.nvim_create_autocmd("FileType", {
  pattern = "dap-float",
  callback = function()
    vim.api.nvim_buf_set_keymap(0, "n", "q", "<cmd>close!<CR>", { noremap = true, silent = true })
  end,
})

vim.api.nvim_set_hl(0, "DapStoppedLine", { default = true, link = "Visual" })

local icons = {
  Stopped = { "󰁕 ", "DiagnosticWarn", "DapStoppedLine" },
  Breakpoint = " ",
  BreakpointCondition = " ",
  BreakpointRejected = { " ", "DiagnosticError" },
  LogPoint = ".>",
}

for name, sign in pairs(icons) do
  sign = type(sign) == "table" and sign or { sign }
  vim.fn.sign_define(
    "Dap" .. name,
    ---@diagnostic disable-next-line: assign-type-mismatch
    { text = sign[1], texthl = sign[2] or "DiagnosticInfo", linehl = sign[3], numhl = sign[3] }
  )
end

local chrome_type = "pwa-chrome"
local node_type = "pwa-node"

require("dap.ext.vscode").json_decode = function(str)
  local config = vim.json.decode(require("plenary.json").json_strip_comments(str))

  for _, item in ipairs(config.configurations) do
    if item.type == "chrome" then
      item.type = chrome_type
    elseif item.type == "node" then
      item.type = node_type
    elseif item.type == "reactnativedirect" then
      item.type = node_type
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
            vim.fn.stdpath("data") .. "/mason/packages/js-debug-adapter/js-debug/src/dapDebugServer.js", "${port}" , "localhost",
          },
  },
}

dap.adapters[node_type] = js_debug_adapter_opts
dap.adapters[chrome_type] = js_debug_adapter_opts

local js_filetypes = { "typescript", "javascript", "typescriptreact", "javascriptreact" }

local vscode = require("dap.ext.vscode")
vscode.type_to_filetypes["node"] = js_filetypes
vscode.type_to_filetypes["pwa-node"] = js_filetypes

for _, language in ipairs(js_filetypes) do
  if not dap.configurations[language] then
    dap.configurations[language] = {
      {
        type = chrome_type,
        name = "React: Attach",
        request = "attach",
        program = "${file}",
        cwd = "${workspaceFolder}",
        sourceMaps = true,
        protocol = "inspector",
        port = 9222,
        webRoot = "${workspaceFolder}",
      },
      {
        type = node_type,
        request = "launch",
        name = "NodeJS: Launch file",
        program = "${file}",
        cwd = "${workspaceFolder}",
      },
      {
        type = node_type,
        request = "attach",
        name = "NodeJS: Attach to 9228",
        cwd = "${workspaceFolder}",
        port = 9228,
      },
      {
        type = node_type,
        request = "attach",
        name = "NodeJS: Pick process",
        processId = require("dap.utils").pick_process,
        cwd = "${workspaceFolder}",
      },
      {
        type = "pwa-node",
        request = "attach",
        name = "Debug React Native App (Advanced Setup)",
        port = 8081,
        address = "localhost",
        localRoot = "${workspaceFolder}",
        remoteRoot = "${workspaceFolder}",
        sourceMaps = true,
        skipFiles = {
          "<node_internals>/**",
          "node_modules/**",
          "**/node_modules/undici/**",
          "**/node_modules/typescript/**",
          "**/node_modules/@expo/**",
          "**/*.bundle.js",
          "**/*.min.js",
        },
        -- Connect via WebSocket protocol used by React Native
        protocol = "inspector",
        timeout = 30000,
      },
    }
  end
end

-- stylua: ignore
local keys = {
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
  { "<leader>dd", function() require("dap").disconnect() end, desc = "Disconnect" },
  { "<leader>dw", function() require("dap.ui.widgets").hover() end, desc = "Widgets" },
  { "<leader>dp", function() require("dap.ui.widgets").centered_float(require("dap.ui.widgets").frames) end, desc = "Scopes" },
}

for _, keymap in pairs(keys) do
  vim.keymap.set("n", keymap[1], keymap[2], { desc = keymap.desc })
end
