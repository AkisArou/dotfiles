--- Shell Sense source for blink.cmp terminal mode.
---
--- The source is a presenter. Shell Sense's live Zsh/Fish/Bash provider owns
--- every candidate and applies every accepted item inside its line editor.
--- @module 'blink.cmp'

local uv = vim.uv or vim.loop

local source = {}
local Client = {}
Client.__index = Client

local clients = {}
local buffer_clients = {}
local autocmds_installed = false
local active_source = nil

local function module_root()
  local file = debug.getinfo(1, "S").source:sub(2)
  return vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(file)))
end

local function default_command()
  if vim.env.SHELL_SENSE_COMMAND and vim.env.SHELL_SENSE_COMMAND ~= "" then
    return vim.env.SHELL_SENSE_COMMAND
  end
  local installed = vim.fn.exepath("shell-sense")
  if installed ~= "" then
    return installed
  end
  local root = module_root()
  for _, profile in ipairs({ "release", "debug" }) do
    local candidate = root .. "/target/" .. profile .. "/shell-sense"
    if vim.fn.executable(candidate) == 1 then
      return candidate
    end
  end
  return "shell-sense"
end

local function notify_error(message)
  vim.schedule(function()
    vim.notify("Shell Sense: " .. message, vim.log.levels.ERROR)
  end)
end

local function completion_key(request_id, generation)
  return tostring(request_id) .. ":" .. tostring(generation)
end

local function optional(value)
  if value == vim.NIL then
    return nil
  end
  return value
end

local function item_key(request_id, generation, item_id)
  return completion_key(request_id, generation) .. ":" .. item_id
end

local function request_line(request)
  local command = optional(request.command)
  if command == nil then
    return nil
  end
  local prefix = command:sub(1, request.cursor)
  local line_start = 1
  local search_from = 1
  while true do
    local newline = prefix:find("\n", search_from, true)
    if newline == nil then
      break
    end
    line_start = newline + 1
    search_from = newline + 1
  end
  return prefix:sub(line_start), line_start - 1
end

local function match_command(waiter, request)
  local line = request_line(request)
  if line == nil then
    return false
  end
  if waiter.found_escape_code then
    return waiter.command == line
  end
  return #waiter.command >= #line and waiter.command:sub(#waiter.command - #line + 1) == line
end

local function request_column_offset(waiter, request)
  local line, line_start = request_line(request)
  if line == nil then
    return nil
  end
  if waiter.found_escape_code then
    return waiter.start_col - line_start, line_start
  end
  return #waiter.command - #line - line_start, line_start
end

local function markup(markup_value)
  markup_value = optional(markup_value)
  if markup_value == nil then
    return nil
  end
  return {
    kind = markup_value.kind == "plain-text" and "plaintext" or markup_value.kind,
    value = markup_value.value,
  }
end

local function blink_item(candidate, list, waiter, request, client)
  local column_offset, line_start = request_column_offset(waiter, request)
  if column_offset == nil or candidate.edit.start < line_start or candidate.edit["end"] < line_start then
    return nil
  end
  local data = {
    shell_sense = {
      process_id = client.shell_process_id,
      request_id = list.request_id,
      generation = list.generation,
      item_id = candidate.id,
      documentation_unresolved = candidate.documentation_unresolved,
      matched = optional(candidate.matched),
    },
  }
  return {
    label = candidate.label,
    labelDetails = {
      detail = optional(candidate.label_detail),
      description = optional(candidate.detail),
    },
    kind = candidate.lsp_kind,
    detail = optional(candidate.detail),
    documentation = markup(candidate.documentation),
    deprecated = candidate.deprecated,
    filterText = optional(candidate.filter_text) or candidate.label,
    sortText = candidate.sort_text,
    insertTextFormat = vim.lsp.protocol.InsertTextFormat.PlainText,
    textEdit = {
      newText = candidate.edit.display_text,
      range = {
        start = { line = 0, character = column_offset + candidate.edit.start },
        ["end"] = { line = 0, character = column_offset + candidate.edit["end"] },
      },
    },
    data = data,
  }
end

function Client.new(shell_process_id, opts)
  local self = setmetatable({
    shell_process_id = shell_process_id,
    opts = opts,
    requests = {},
    settled = {},
    completion_waiters = {},
    documentation_waiters = {},
    selection_waiters = {},
    active_key = nil,
    read_buffer = "",
    stderr = "",
    ready = false,
    closed = false,
  }, Client)

  self.stdin = assert(uv.new_pipe(false))
  self.stdout = assert(uv.new_pipe(false))
  self.stderr_pipe = assert(uv.new_pipe(false))
  local arguments = { "blink", "--shell-process-id", tostring(shell_process_id) }
  vim.list_extend(arguments, { "--attach-timeout-ms", tostring(opts.attach_timeout_ms) })
  if opts.socket then
    vim.list_extend(arguments, { "--socket", opts.socket })
  end
  local handle, process_id_or_error = uv.spawn(opts.command, {
    args = arguments,
    stdio = { self.stdin, self.stdout, self.stderr_pipe },
  }, function(code, signal)
    vim.schedule(function()
      self:close_handles()
      if not self.closed and code ~= 0 then
        local detail = self.stderr ~= "" and self.stderr or ("exit " .. code .. ", signal " .. signal)
        notify_error("Blink bridge stopped: " .. detail)
      end
      self.closed = true
      self:finish_waiters()
      clients[self.shell_process_id] = nil
    end)
  end)
  if not handle then
    self:close_handles()
    error("could not start " .. opts.command .. ": " .. tostring(process_id_or_error))
  end
  self.handle = handle
  self.process_id = process_id_or_error

  self.stdout:read_start(function(error, chunk)
    if error then
      notify_error(error)
      return
    end
    if chunk then
      self:on_stdout(chunk)
    end
  end)
  self.stderr_pipe:read_start(function(_, chunk)
    if chunk and #self.stderr < 8192 then
      self.stderr = (self.stderr .. chunk):sub(1, 8192)
    end
  end)
  return self
end

function Client:on_stdout(chunk)
  self.read_buffer = self.read_buffer .. chunk
  while true do
    local newline = self.read_buffer:find("\n", 1, true)
    if newline == nil then
      return
    end
    local line = self.read_buffer:sub(1, newline - 1)
    self.read_buffer = self.read_buffer:sub(newline + 1)
    if line ~= "" then
      local ok, event = pcall(vim.json.decode, line)
      if ok then
        vim.schedule(function()
          self:on_event(event)
        end)
      else
        notify_error("invalid bridge event: " .. tostring(event))
      end
    end
  end
end

function Client:on_event(event)
  if self.closed then
    return
  end
  if event.type == "ready" then
    self.ready = true
    return
  end
  if event.type == "request" then
    local key = completion_key(event.request_id, event.generation)
    if self.active_key ~= key then
      self.active_key = key
      self.requests = {}
      self.settled = {}
      self:finish_documentation_waiters()
    end
    self.requests[key] = event
    self:dispatch_completions(key)
    return
  end
  if event.type == "completions" then
    local key = completion_key(event.request_id, event.generation)
    if key ~= self.active_key then
      return
    end
    if event.is_settled then
      self.settled[key] = event
    end
    self:dispatch_completions(key)
    return
  end
  if event.type == "request-cancelled" then
    local key = completion_key(event.request_id, event.generation)
    if key ~= self.active_key then
      return
    end
    self.active_key = nil
    self.requests[key] = nil
    self.settled[key] = nil
    self:finish_documentation_waiters()
    return
  end
  if event.type == "documentation" then
    local key = item_key(event.request_id, event.generation, event.item_id)
    self:finish_documentation(key, event)
    return
  end
  if event.type == "selection-finished" then
    local key = item_key(event.request_id, event.generation, event.item_id)
    local failure_message = nil
    if not event.applied then
      failure_message = "the native shell rejected the selected completion"
    end
    self:finish_selection(key, failure_message)
    return
  end
  if event.type == "error" then
    local request_id = optional(event.request_id)
    if request_id then
      for key, waiter in pairs(self.selection_waiters) do
        if waiter.request_id == request_id then
          self:finish_selection(key, nil)
        end
      end
    end
    notify_error(event.code .. ": " .. event.message)
  end
end

function Client:finish_selection(key, failure_message)
  local waiter = self.selection_waiters[key]
  self.selection_waiters[key] = nil
  if waiter and not waiter.done then
    waiter.done = true
    if failure_message then
      notify_error(failure_message)
    end
    waiter.callback()
  end
end

function Client:finish_documentation(key, event)
  local waiters = self.documentation_waiters[key] or {}
  self.documentation_waiters[key] = nil
  for _, waiter in ipairs(waiters) do
    if not waiter.done then
      waiter.done = true
      if event then
        waiter.item.documentation = markup(event.documentation)
        waiter.item.data.shell_sense.documentation_unresolved = event.unresolved
      end
      waiter.callback(waiter.item)
    end
  end
end

function Client:finish_documentation_waiters()
  local keys = vim.tbl_keys(self.documentation_waiters)
  for _, key in ipairs(keys) do
    self:finish_documentation(key, nil)
  end
end

function Client:dispatch_completions(key)
  local request = self.requests[key]
  local list = self.settled[key]
  if request == nil or list == nil then
    return
  end
  for waiter_id, waiter in pairs(self.completion_waiters) do
    if match_command(waiter, request) then
      self.completion_waiters[waiter_id] = nil
      local items = {}
      for _, candidate in ipairs(list.items) do
        local item = blink_item(candidate, list, waiter, request, self)
        if item then
          table.insert(items, item)
        end
      end
      waiter.callback({
        items = items,
        -- Shell-native validity may change after any edit, in either direction.
        is_incomplete_forward = true,
        is_incomplete_backward = true,
      })
    end
  end
end

function Client:add_completion_waiter(command, callback)
  local waiter_id = tostring(uv.hrtime())
  local waiter = {
    command = command.text,
    start_col = command.start_col,
    found_escape_code = command.found_escape_code,
    callback = callback,
  }
  self.completion_waiters[waiter_id] = waiter
  for key in pairs(self.settled) do
    self:dispatch_completions(key)
  end
  return function()
    self.completion_waiters[waiter_id] = nil
  end
end

function Client:send(command)
  if self.closed or self.stdin == nil or self.stdin:is_closing() then
    return false
  end
  self.stdin:write(vim.json.encode(command) .. "\n")
  return true
end

function Client:resolve(item, callback)
  local data = item.data and item.data.shell_sense
  if data == nil or (item.documentation ~= nil and not data.documentation_unresolved) then
    callback(item)
    return
  end
  local key = item_key(data.request_id, data.generation, data.item_id)
  local waiter = { item = vim.deepcopy(item), callback = callback, done = false }
  self.documentation_waiters[key] = self.documentation_waiters[key] or {}
  table.insert(self.documentation_waiters[key], waiter)
  if
    not self:send({
      type = "resolve",
      request_id = data.request_id,
      generation = data.generation,
      item_id = data.item_id,
    })
  then
    self:finish_documentation(key, nil)
    return
  end
  vim.defer_fn(function()
    if waiter.done then
      return
    end
    self:finish_documentation(key, nil)
  end, self.opts.resolve_timeout_ms)
end

function Client:select(item, callback)
  local data = item.data and item.data.shell_sense
  if data == nil then
    callback()
    return
  end
  local key = item_key(data.request_id, data.generation, data.item_id)
  local waiter = { callback = callback, done = false, request_id = data.request_id }
  self.selection_waiters[key] = waiter
  if
    not self:send({
      type = "select",
      request_id = data.request_id,
      generation = data.generation,
      item_id = data.item_id,
    })
  then
    waiter.done = true
    self.selection_waiters[key] = nil
    callback()
    return
  end
  vim.defer_fn(function()
    if waiter.done then
      return
    end
    notify_error("native completion acceptance timed out")
    self:finish_selection(key, nil)
  end, self.opts.accept_timeout_ms)
end

function Client:finish_waiters()
  for id, waiter in pairs(self.completion_waiters) do
    self.completion_waiters[id] = nil
    waiter.callback({ items = {}, is_incomplete_forward = true, is_incomplete_backward = true })
  end
  self:finish_documentation_waiters()
  for key, waiter in pairs(self.selection_waiters) do
    self.selection_waiters[key] = nil
    if not waiter.done then
      waiter.done = true
      waiter.callback()
    end
  end
end

function Client:close_handles()
  for _, stream in ipairs({ self.stdin, self.stdout, self.stderr_pipe }) do
    if stream and not stream:is_closing() then
      stream:close()
    end
  end
  if self.handle and not self.handle:is_closing() then
    self.handle:close()
  end
end

function Client:close()
  if self.closed then
    return
  end
  self:send({ type = "goodbye" })
  self.closed = true
  self:finish_waiters()
  self:close_handles()
end

local function terminal_shell_pid(bufnr)
  local job_id = vim.b[bufnr].terminal_job_id
  if type(job_id) ~= "number" then
    return nil
  end
  local process_id = vim.fn.jobpid(job_id)
  if type(process_id) ~= "number" or process_id <= 0 then
    return nil
  end
  return process_id
end

local function install_autocmds()
  if autocmds_installed then
    return
  end
  autocmds_installed = true
  local group = vim.api.nvim_create_augroup("shell-sense-blink", { clear = true })
  vim.api.nvim_create_autocmd("TermOpen", {
    group = group,
    callback = function(event)
      vim.schedule(function()
        if active_source and vim.api.nvim_buf_is_valid(event.buf) then
          active_source:client(event.buf)
        end
      end)
    end,
  })
  vim.api.nvim_create_autocmd({ "TermClose", "BufWipeout" }, {
    group = group,
    callback = function(event)
      local process_id = buffer_clients[event.buf]
      buffer_clients[event.buf] = nil
      local client = process_id and clients[process_id] or nil
      if client then
        clients[process_id] = nil
        client:close()
      end
    end,
  })
  vim.api.nvim_create_autocmd("VimLeavePre", {
    group = group,
    callback = function()
      for process_id, client in pairs(clients) do
        clients[process_id] = nil
        client:close()
      end
    end,
  })
end

function source.new(opts)
  opts = vim.tbl_deep_extend("force", {
    command = default_command(),
    socket = nil,
    attach_timeout_ms = 3000,
    accept_timeout_ms = 2000,
    resolve_timeout_ms = 2000,
  }, opts or {})
  vim.validate("shell-sense.command", opts.command, "string")
  vim.validate("shell-sense.socket", opts.socket, { "string", "nil" })
  vim.validate("shell-sense.attach_timeout_ms", opts.attach_timeout_ms, "number")
  vim.validate("shell-sense.accept_timeout_ms", opts.accept_timeout_ms, "number")
  vim.validate("shell-sense.resolve_timeout_ms", opts.resolve_timeout_ms, "number")
  install_autocmds()
  local instance = setmetatable({ opts = opts }, { __index = source })
  active_source = instance
  return instance
end

function source:enabled()
  return vim.bo.buftype == "terminal"
end

function source:client(bufnr)
  local process_id = terminal_shell_pid(bufnr)
  if process_id == nil then
    return nil
  end
  buffer_clients[bufnr] = process_id
  local client = clients[process_id]
  if client and not client.closed then
    return client
  end
  local ok, created = pcall(Client.new, process_id, self.opts)
  if not ok then
    notify_error(created)
    return nil
  end
  clients[process_id] = created
  return created
end

function source:get_completions(ctx, callback)
  local command = ctx.term and ctx.term.command
  local client = command and self:client(ctx.bufnr) or nil
  if client == nil then
    callback({ items = {}, is_incomplete_forward = true, is_incomplete_backward = true })
    return
  end
  return client:add_completion_waiter(command, callback)
end

function source:resolve(item, callback)
  local data = item.data and item.data.shell_sense
  local client = data and clients[data.process_id] or nil
  if client == nil then
    callback(item)
    return
  end
  client:resolve(item, callback)
end

function source:execute(_, item, callback, _)
  local data = item.data and item.data.shell_sense
  local client = data and clients[data.process_id] or nil
  if client == nil then
    notify_error("the owning shell session is no longer attached")
    callback()
    return
  end
  client:select(item, callback)
end

return source
