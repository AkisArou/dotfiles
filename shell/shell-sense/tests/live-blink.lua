local project_root = assert(vim.env.SHELL_SENSE_TEST_ROOT)
local result_path = assert(vim.env.SHELL_SENSE_BLINK_RESULT)
local work_dir = assert(vim.env.SHELL_SENSE_TEST_WORK)
local blink_cmp_root = assert(vim.env.SHELL_SENSE_BLINK_CMP_ROOT)
local blink_lib_root = assert(vim.env.SHELL_SENSE_BLINK_LIB_ROOT)

vim.opt.runtimepath:prepend(project_root)
vim.opt.runtimepath:prepend(blink_cmp_root)
vim.opt.runtimepath:prepend(blink_lib_root)
vim.o.lines = 40
vim.o.columns = 160
vim.o.virtualedit = "onemore"
local notifications = {}
vim.notify = function(message, level)
  table.insert(notifications, { message = tostring(message), level = level })
end

local function finish(ok, message)
  local status = ok and "live-blink-ok" or ("live-blink-failed\n" .. message)
  vim.fn.writefile(vim.split(status, "\n", { plain = true }), result_path)
  vim.cmd(ok and "qa!" or "cquit 1")
end

local function wait_for(predicate, message, timeout_ms)
  local ok = vim.wait(timeout_ms or 5000, predicate, 10)
  assert(ok, message)
end

local function terminal_text(bufnr)
  return table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), "\n")
end

local function follow_terminal_cursor(bufnr, command, cursor)
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  for index = #lines, 1, -1 do
    if vim.startswith(lines[index], "BLINK> " .. command) then
      vim.api.nvim_win_set_cursor(0, { index, #"BLINK> " + cursor })
      return
    end
  end
  error("could not locate the live command in the terminal buffer")
end

local function current_request(bridge, command)
  local request
  wait_for(function()
    request = bridge.active_key and bridge.requests[bridge.active_key] or nil
    return request ~= nil and request.command == command
  end, "the presentation bridge did not receive " .. command)
  return request
end

local function labels(items)
  return vim.tbl_map(function(item)
    return item.label
  end, items)
end

local function find_item(items, label)
  for index, item in ipairs(items) do
    if item.label == label then
      return index, item
    end
  end
end

local function terminal_mapping(bufnr, lhs)
  local expected = vim.keycode(lhs)
  for _, mapping in ipairs(vim.api.nvim_buf_get_keymap(bufnr, "t")) do
    if vim.keycode(mapping.lhs) == expected then
      return mapping
    end
  end
end

local cmp = require("blink.cmp")
cmp.setup({
  fuzzy = { implementation = "lua" },
  keymap = {
    preset = "default",
    ["<C-e>"] = { "select_and_accept" },
  },
  -- The synchronous `-l` harness pins Blink's mode probe after setup, so keep
  -- the terminal selection policy identical in the base configuration too.
  completion = {
    list = { selection = { preselect = true, auto_insert = false } },
    menu = { auto_show = true },
    documentation = { auto_show = false },
    ghost_text = { enabled = false },
  },
  sources = {
    default = { "shell_sense" },
    providers = {
      shell_sense = {
        name = "Shell Sense",
        module = "blink-cmp-shell-sense",
        async = true,
        timeout_ms = 3000,
        opts = {
          command = assert(vim.env.SHELL_SENSE_COMMAND),
          socket = assert(vim.env.SHELL_SENSE_SOCKET),
          attach_timeout_ms = 5000,
          accept_timeout_ms = 3000,
          resolve_timeout_ms = 3000,
        },
      },
    },
  },
  term = {
    enabled = true,
    sources = { default = { "shell_sense" } },
    completion = {
      list = { selection = { preselect = true, auto_insert = false } },
      menu = { auto_show = true },
      documentation = { auto_show = false },
      ghost_text = { enabled = false },
    },
  },
})

vim.cmd("enew")
local terminal_buffer = vim.api.nvim_get_current_buf()
local terminal_job = vim.fn.jobstart({ "/usr/bin/zsh", "-d", "-i" }, {
  term = true,
  cwd = work_dir,
  env = {
    HOME = vim.env.HOME,
    LANG = "C.UTF-8",
    PATH = vim.env.PATH,
    SHELL = "/usr/bin/zsh",
    TERM = "xterm-256color",
    ZDOTDIR = project_root .. "/tests/fixtures/blink-zdotdir",
    XDG_RUNTIME_DIR = assert(vim.env.XDG_RUNTIME_DIR),
    XDG_STATE_HOME = assert(vim.env.XDG_STATE_HOME),
    SHELL_SENSE_COMMAND = assert(vim.env.SHELL_SENSE_COMMAND),
    SHELL_SENSE_CONFIG = assert(vim.env.SHELL_SENSE_CONFIG),
    SHELL_SENSE_SOCKET = assert(vim.env.SHELL_SENSE_SOCKET),
    SHELL_SENSE_NO_DAEMON_AUTOSTART = "1",
    SHELL_SENSE_TEST_ROOT = project_root,
  },
})
assert(terminal_job > 0, "could not start the terminal Zsh")

-- `nvim -l` executes the test as one synchronous Lua chunk, so Neovim cannot
-- transition from terminal-normal (`nt`) to terminal-input (`t`) until the
-- chunk returns. The buffer and terminal job are real; only Blink's mode probe
-- is pinned to the mode this test is exercising.
local terminal_mode = function()
  return { mode = "t", blocking = false }
end
vim.api.nvim_get_mode = terminal_mode
require("blink.lib.nvim").get_mode = terminal_mode
require("blink.cmp.keymap").ensure_mappings()

vim.wait(150)
local ok, failure = xpcall(function()
  assert(vim.bo[terminal_buffer].buftype == "terminal", "Neovim did not create a terminal buffer")
  wait_for(function()
    return terminal_text(terminal_buffer):find("<BLINK%-SHELL%-READY/>") ~= nil
  end, "the terminal Zsh did not initialize")
  local provider = require("blink.cmp.sources.lib").get_provider_by_id("shell_sense")
  local bridge = provider.module:client(terminal_buffer)
  wait_for(function()
    return bridge.ready and not bridge.closed
  end, "the Blink presentation bridge did not attach to the live shell")

  -- The editor bridge attaches to the exact live shell process. Once attached,
  -- native ZLE presentation is suppressed and Blink is the sole presenter.
  vim.fn.chansend(terminal_job, "blink-test --a")
  wait_for(function()
    return terminal_text(terminal_buffer):find("BLINK> blink%-test %-%-a") ~= nil
  end, "the option query did not reach ZLE")
  local first_request = current_request(bridge, "blink-test --a")
  follow_terminal_cursor(terminal_buffer, first_request.command, first_request.cursor)

  assert(cmp.show({ providers = { "shell_sense" }, initial_selected_item_idx = 1 }))
  local menu_ready = vim.wait(5000, function()
    return cmp.is_menu_visible() and #cmp.get_items() == 2
  end, 10)
  assert(
    menu_ready,
    "Blink did not show the two native option candidates\nitems="
      .. vim.inspect(cmp.get_items())
      .. "\ncontext="
      .. vim.inspect(cmp.get_context())
      .. "\nnotifications="
      .. vim.inspect(notifications)
      .. "\nterminal="
      .. terminal_text(terminal_buffer)
  )

  local first_items = cmp.get_items()
  local first_labels = labels(first_items)
  table.sort(first_labels)
  assert(vim.deep_equal(first_labels, { "--all", "--amend" }), "Blink received the wrong native candidates")
  local amend_index, amend = find_item(first_items, "--amend")
  assert(amend_index and amend, "the --amend candidate is missing")
  assert(amend.kind == vim.lsp.protocol.CompletionItemKind.Keyword, "the option LSP kind is wrong")
  assert(amend.labelDetails.description == "replace the previous commit", "the native description was lost")
  local first_generation = amend.data.shell_sense.generation
  assert(
    not terminal_text(terminal_buffer):find("replace the previous commit", 1, true),
    "the shell popup remained visible after Blink attached"
  )

  -- An edit creates a new native generation. A hidden stale Blink request may
  -- not republish its old two-item result over the new one.
  cmp.cancel()
  vim.fn.chansend(terminal_job, "\21blink-test --am")
  local second_query_ready = vim.wait(5000, function()
    return terminal_text(terminal_buffer):find("BLINK> blink%-test %-%-am") ~= nil
  end, 10)
  assert(second_query_ready, "the second option query did not reach ZLE\nterminal=" .. terminal_text(terminal_buffer))
  local second_request = current_request(bridge, "blink-test --am")
  follow_terminal_cursor(terminal_buffer, second_request.command, second_request.cursor)
  assert(cmp.show({ providers = { "shell_sense" }, initial_selected_item_idx = 1 }))
  local second_menu_ready = vim.wait(5000, function()
    local items = cmp.get_items()
    return cmp.is_menu_visible()
      and #items == 1
      and items[1].label == "--amend"
      and items[1].data.shell_sense.generation > first_generation
  end, 10)
  assert(
    second_menu_ready,
    "Blink did not replace the stale generation atomically\nitems="
      .. vim.inspect(cmp.get_items())
      .. "\ncontext="
      .. vim.inspect(cmp.get_context())
      .. "\nterminal="
      .. terminal_text(terminal_buffer)
  )

  -- Blink resolves before execute. Move ZLE to a newer generation while its
  -- prior item is still selected, then exercise the real inherited terminal
  -- mapping. The source must rebase the unambiguous native candidate instead
  -- of resolving or selecting the stale generation.
  local stale_item = cmp.get_items()[1]
  vim.fn.chansend(terminal_job, "\21blink-test --ame")
  wait_for(function()
    return terminal_text(terminal_buffer):find("BLINK> blink%-test %-%-ame") ~= nil
  end, "the acceptance-race query did not reach ZLE")
  local acceptance_request = current_request(bridge, "blink-test --ame")
  assert(
    acceptance_request.generation > stale_item.data.shell_sense.generation,
    "the acceptance test did not cross a native generation boundary"
  )
  follow_terminal_cursor(terminal_buffer, acceptance_request.command, acceptance_request.cursor)

  local stale_resolved = false
  provider.module:resolve(stale_item, function()
    stale_resolved = true
  end)
  wait_for(function()
    return stale_resolved
  end, "the stale documentation callback did not finish as a cancellation")

  local stale_errors = 0
  local on_event = bridge.on_event
  bridge.on_event = function(client, event)
    if event.type == "error" and event.code == "stale-request" then
      stale_errors = stale_errors + 1
    end
    on_event(client, event)
  end
  assert(
    bridge:send({
      type = "resolve",
      request_id = stale_item.data.shell_sense.request_id,
      generation = stale_item.data.shell_sense.generation,
      item_id = stale_item.data.shell_sense.item_id,
    }),
    "the stale documentation regression request was not sent"
  )
  wait_for(function()
    return stale_errors == 1
  end, "the daemon did not reject the stale documentation regression request")

  local control_e = terminal_mapping(terminal_buffer, "<C-e>")
  assert(control_e and type(control_e.callback) == "function", "Blink did not install the terminal <C-e> mapping")
  control_e.callback()
  local acceptance_finished = vim.wait(5000, function()
    return terminal_text(terminal_buffer):find("BLINK> blink%-test %-%-amend ") ~= nil
  end, 10)
  assert(
    acceptance_finished,
    "native completion acceptance did not finish\nselection-waiters="
      .. vim.inspect(bridge.selection_waiters)
      .. "\nnotifications="
      .. vim.inspect(notifications)
      .. "\nterminal="
      .. terminal_text(terminal_buffer)
  )
  assert(
    not terminal_text(terminal_buffer):find("%-%-amendamend"),
    "Blink applied its display edit in addition to native shell acceptance"
  )
  assert(not vim.iter(notifications):any(function(notification)
    return notification.message:find("stale%-request") ~= nil
  end), "Blink exposed an expected stale documentation cancellation")

  -- Path documentation remains lazy. Blink resolves it through the daemon and
  -- renders the configured directory resolver in its documentation window.
  vim.fn.chansend(terminal_job, "\21cd dotf")
  wait_for(function()
    return terminal_text(terminal_buffer):find("BLINK> cd dotf") ~= nil
  end, "the directory query did not reach ZLE")
  local path_request = current_request(bridge, "cd dotf")
  follow_terminal_cursor(terminal_buffer, path_request.command, path_request.cursor)
  assert(cmp.show({ providers = { "shell_sense" }, initial_selected_item_idx = 1 }))
  wait_for(function()
    local items = cmp.get_items()
    return cmp.is_menu_visible() and find_item(items, "dotfiles") ~= nil
  end, "Blink did not receive the native directory candidate")
  local path_items = cmp.get_items()
  local path_index, path_item = find_item(path_items, "dotfiles")
  assert(path_item.kind == vim.lsp.protocol.CompletionItemKind.Folder, "the directory LSP kind is wrong")
  assert(
    path_item.data.shell_sense.documentation_unresolved,
    "capability-backed path documentation was not exposed as unresolved\nitem=" .. vim.inspect(path_item)
  )

  cmp.cancel()
  assert(cmp.show({ providers = { "shell_sense" }, initial_selected_item_idx = path_index }))
  wait_for(function()
    return cmp.is_menu_visible() and cmp.get_selected_item_idx() == path_index
  end, "Blink did not restore the directory selection")
  assert(cmp.show_documentation(), "Blink refused to resolve path documentation")
  wait_for(function()
    if not cmp.is_documentation_visible() then
      return false
    end
    local docs = require("blink.cmp.completion.windows.documentation")
    local text = table.concat(vim.api.nvim_buf_get_lines(docs.win:get_buf(), 0, -1, false), "\n")
    return text:find("nvim", 1, true) ~= nil
  end, "Blink did not render resolved directory documentation")

  vim.api.nvim_buf_delete(terminal_buffer, { force = true })
end, debug.traceback)
finish(ok, failure or "")
