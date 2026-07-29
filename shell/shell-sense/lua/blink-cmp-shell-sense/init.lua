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

local function candidate_signature(candidate)
  return {
    label = candidate.label,
    kind = candidate.kind,
    source = candidate.source,
    group = optional(candidate.group),
  }
end

local function same_signature(left, right)
  return left ~= nil
    and right ~= nil
    and left.label == right.label
    and left.kind == right.kind
    and left.source == right.source
    and left.group == right.group
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
      signature = candidate_signature(candidate),
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
    selection_sequence = 0,
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
    self:dispatch_pending_selections()
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
    local failure_message = nil
    if not event.applied then
      failure_message = "the native shell rejected the selected completion"
    end
    for _, waiter in ipairs(vim.tbl_values(self.selection_waiters)) do
      if
        waiter.request_id == event.request_id
        and waiter.generation == event.generation
        and waiter.item_id == event.item_id
      then
        self:finish_selection(waiter, failure_message)
      end
    end
    return
  end
  if event.type == "error" then
    local request_id = optional(event.request_id)
    if event.code == "stale-request" then
      self:finish_documentation_waiters(request_id)
      for _, waiter in ipairs(vim.tbl_values(self.selection_waiters)) do
        if waiter.request_id == request_id then
          waiter.stale_key = completion_key(waiter.request_id, waiter.generation)
          waiter.sent = false
          waiter.request_id = nil
          waiter.generation = nil
          waiter.item_id = nil
        end
      end
      self:dispatch_pending_selections()
      return
    end
    if request_id then
      for _, waiter in ipairs(vim.tbl_values(self.selection_waiters)) do
        if waiter.request_id == request_id then
          self:finish_selection(waiter, nil)
        end
      end
    end
    notify_error(event.code .. ": " .. event.message)
  end
end

function Client:finish_selection(waiter, failure_message)
  if waiter.done then
    return
  end
  waiter.done = true
  self.selection_waiters[waiter.id] = nil
  if failure_message then
    notify_error(failure_message)
  end
  waiter.callback()
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

function Client:finish_documentation_waiters(request_id)
  local keys = vim.tbl_keys(self.documentation_waiters)
  for _, key in ipairs(keys) do
    local waiters = self.documentation_waiters[key]
    if request_id == nil or (waiters[1] and waiters[1].request_id == request_id) then
      self:finish_documentation(key, nil)
    end
  end
end

function Client:rebase_item(item)
  local data = item.data and item.data.shell_sense
  if data == nil or self.active_key == nil then
    return nil, "invalid"
  end
  if completion_key(data.request_id, data.generation) == self.active_key then
    return item, "ready"
  end

  local list = self.settled[self.active_key]
  if list == nil then
    return nil, "pending"
  end
  local exact = nil
  local semantic = nil
  local semantic_count = 0
  for _, candidate in ipairs(list.items) do
    if candidate.id == data.item_id then
      exact = candidate
      break
    end
    if same_signature(data.signature, candidate_signature(candidate)) then
      semantic = candidate
      semantic_count = semantic_count + 1
    end
  end
  local candidate = exact or (semantic_count == 1 and semantic or nil)
  if candidate == nil then
    return nil, "invalid"
  end

  local rebased = vim.deepcopy(item)
  local rebased_data = rebased.data.shell_sense
  rebased_data.request_id = list.request_id
  rebased_data.generation = list.generation
  rebased_data.item_id = candidate.id
  rebased_data.documentation_unresolved = candidate.documentation_unresolved
  rebased_data.matched = optional(candidate.matched)
  rebased_data.signature = candidate_signature(candidate)
  rebased.documentation = markup(candidate.documentation)
  return rebased, "ready"
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
  local rebased, state = self:rebase_item(item)
  if state ~= "ready" then
    callback(item)
    return
  end
  item = rebased
  local data = item.data.shell_sense
  if item.documentation ~= nil and not data.documentation_unresolved then
    callback(item)
    return
  end
  local key = item_key(data.request_id, data.generation, data.item_id)
  local waiter = {
    item = vim.deepcopy(item),
    callback = callback,
    done = false,
    request_id = data.request_id,
  }
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

function Client:dispatch_selection(waiter)
  if waiter.done or waiter.sent then
    return
  end
  if waiter.stale_key == self.active_key then
    return
  end
  local item, state = self:rebase_item(waiter.item)
  if state == "pending" then
    return
  end
  if state ~= "ready" then
    self:finish_selection(waiter, "the selected native completion is no longer available")
    return
  end
  waiter.item = item
  local data = item.data.shell_sense
  waiter.request_id = data.request_id
  waiter.generation = data.generation
  waiter.item_id = data.item_id
  waiter.stale_key = nil
  waiter.sent = true
  if
    not self:send({
      type = "select",
      request_id = data.request_id,
      generation = data.generation,
      item_id = data.item_id,
    })
  then
    self:finish_selection(waiter, nil)
  end
end

function Client:dispatch_pending_selections()
  for _, waiter in ipairs(vim.tbl_values(self.selection_waiters)) do
    self:dispatch_selection(waiter)
  end
end

function Client:select(item, callback)
  self.selection_sequence = self.selection_sequence + 1
  local waiter = {
    id = self.selection_sequence,
    item = vim.deepcopy(item),
    callback = callback,
    done = false,
    sent = false,
  }
  self.selection_waiters[waiter.id] = waiter
  self:dispatch_selection(waiter)
  vim.defer_fn(function()
    if waiter.done then
      return
    end
    notify_error("native completion acceptance timed out")
    self:finish_selection(waiter, nil)
  end, self.opts.accept_timeout_ms)
end

function Client:finish_waiters()
  for id, waiter in pairs(self.completion_waiters) do
    self.completion_waiters[id] = nil
    waiter.callback({ items = {}, is_incomplete_forward = true, is_incomplete_backward = true })
  end
  self:finish_documentation_waiters()
  local selection_waiters = vim.tbl_values(self.selection_waiters)
  for _, waiter in ipairs(selection_waiters) do
    self:finish_selection(waiter, nil)
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
