local use_fidget = false

local fidget_notifier = {}

fidget_notifier.on_start = function(package_name)
  local fidget = require("fidget")
  fidget.notify("Installing " .. package_name, nil, { key = package_name })
end

fidget_notifier.on_output = function(package_name, line)
  local fidget = require("fidget")
  fidget.notify(line .. package_name, nil, { key = package_name })
end

fidget_notifier.on_error = function(package_name)
  local fidget = require("fidget")
  fidget.notify("Installation error: " .. package_name, nil, { key = package_name })
end

fidget_notifier.on_success = function(package_name)
  local fidget = require("fidget")
  fidget.notify("Installation success: " .. package_name, nil, { key = package_name })
end

local nvim_notifier = {}

nvim_notifier.on_start = function(package_name)
  vim.notify("Installing " .. package_name)
end

nvim_notifier.on_output = function(package_name, line)
  vim.notify(line .. package_name)
end

nvim_notifier.on_error = function(package_name)
  vim.notify("Installation error: " .. package_name)
end

nvim_notifier.on_success = function(package_name)
  vim.notify("Installation success: " .. package_name)
end

local function get_notifier()
  if use_fidget then
    return fidget_notifier
  end

  return nvim_notifier
end

-- Function to execute the pnpm command
function Run_pnpm_install()
  -- Get the current buffer's file path
  local file_path = vim.api.nvim_buf_get_name(0)
  if file_path:match("package%.json$") then
    -- Read the package.json file
    local file = io.open(file_path, "r")

    if file == nil then
      return
    end

    local content = file:read("*a")
    file:close()

    -- Parse the JSON content
    local json = vim.fn.json_decode(content)

    -- Get the package name
    local package_name = json.name

    if package_name == nil then
      print("Not a package.json file")
      return
    end

    -- Form the command
    local command = "pnpm install --offline --filter " .. package_name

    local notifier = get_notifier()
    -- Create a new task for fidget

    notifier.on_start(package_name)

    -- Function to handle command output
    local function on_output(_, data, _)
      if data then
        for _, line in ipairs(data) do
          if line ~= "" then
            notifier.on_output(package_name, line)
          end
        end
      end
    end

    -- Run the command and capture the output
    vim.fn.jobstart(command, {
      stdout_buffered = false,
      on_stdout = on_output,
      on_stderr = on_output,
      on_exit = function(_, exit_code, _)
        if exit_code == 0 then
          notifier.on_success(package_name)
        else
          notifier.on_error(package_name)
        end
      end,
    })

    print("Running: " .. command)
  else
    print("Package name not found in package.json")
  end
end

-- Create an augroup
local augroup = vim.api.nvim_create_augroup("PnpmInstall", { clear = true })

-- Create an autocmd for the FileType event
vim.api.nvim_create_autocmd("FileType", {
  group = augroup,
  pattern = { "json", "packagejson" },
  callback = function()
    -- Set the keybinding in normal mode
    vim.api.nvim_buf_set_keymap(
      0,
      "n",
      "<leader>ci",
      ":lua Run_pnpm_install()<CR>",
      { noremap = true, silent = true, desc = "pnpm install for workspace" }
    )
  end,
})
