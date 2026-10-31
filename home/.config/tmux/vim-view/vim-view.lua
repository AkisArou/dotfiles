-- Vim view: a tmux pane's scrollback as a Neovim buffer, floating over the pane.
--
-- This Neovim runs headless, outside tmux, and waits for a view. The vim-view
-- launcher floats a borderless pane over the pane to view, at its exact place
-- and size, and attaches a UI to a waiting Neovim; the program underneath
-- keeps running untouched. Its new output keeps arriving, and the view follows
-- when the cursor is on the last line, as in a Neovim terminal. q, i, a, I or
-- A close the view. The buffer's filetype is vim-view.
--
-- Each view has its own Neovim: the launcher claims a waiting one and starts
-- the next. When a view ends, this Neovim waits again, unless another one
-- already does.
--
-- vim-view.so, built from vim-view.c, parses what tmux captures; this file
-- keeps what needs Neovim. The program's output reaches this Neovim through
-- pipe-pane, which only tells it to read the pane again.
--
-- Experimental.

local api, uv = vim.api, vim.uv
local ffi, bit = require('ffi'), require('bit')
local M = {}
_G.VimView = M

pcall(ffi.cdef, [[
typedef struct { uint32_t start, stop; int32_t attr; } vv_span;
typedef struct { int32_t fg, bg, sp; uint32_t flags; } vv_attr;
void vv_clear(long pane_width, long view_width);
long vv_parse(const char *data, size_t size, long first, int prepend);
long vv_replace(const char *data, size_t size, long first, long *old_end, long *new_end);
int vv_open_fifo(const char *path);
long vv_drain(int fd);
long vv_colour(const char *data, size_t size, long first, long stop);
long vv_chunk(long stop, long most, long *taken);
long vv_count(void);
const char *vv_text(long i, size_t *length);
int vv_spans(long i, const vv_span **spans);
const vv_attr *vv_attribute(int id);
long vv_tail(long from, long height, long total_rows, long *anchor);
long vv_locate(long first, long anchor, long target, long *offset);
]])
local C = ffi.load(vim.fs.dirname(debug.getinfo(1, 'S').source:sub(2)) .. '/vim-view.so')
local FILL = 0xFFFFFFFF
local size, number_out, spans_out = ffi.new('size_t[1]'), ffi.new('long[1]'), ffi.new('const vv_span *[1]')
local end_out = ffi.new('long[1]')

-- The vim-view launcher's pool: see there.
local pool, name = vim.env.VIM_VIEW_POOL, vim.env.VIM_VIEW_NAME
-- pipe-pane writes the viewed program's output here.
local fifo_path = pool .. '/' .. name .. '.fifo'

local ns = api.nvim_create_namespace('vim-view')
local view = { generation = 0 } -- What the buffer holds: for which pane, in which floating pane.

local log_path = vim.env.VIM_VIEW_LOG
local function log(...)
  if log_path then
    local f = io.open(log_path, 'a')
    f:write(('%.1f %s '):format(uv.hrtime() / 1e6, vim.env.VIM_VIEW_NAME or ''), table.concat(vim.tbl_map(tostring, { ... }), ' '), '\n')
    f:close()
  end
end

-- tmux commands --------------------------------------------------------------

-- Nothing here attaches to a tmux session: tmux counts every attached client,
-- control-mode ones too, as looking at its session's window, and that
-- window's pane would never hear that focus left the terminal.
-- Run tmux commands; with a callback, without waiting for them. tmux skips
-- the rest of a command list after a failing command.
local function tmux(args, callback)
  local command = vim.list_extend({ 'tmux' }, args)
  if not callback then
    local result = vim.system(command):wait()
    return result.code == 0, result.stdout or ''
  end
  vim.system(command, {}, function(result)
    vim.schedule(function()
      callback(result.code == 0, result.stdout or '')
    end)
  end)
end

-- Colours --------------------------------------------------------------------

local XTERM = { '#000000', '#cd0000', '#00cd00', '#cdcd00', '#0000ee', '#cd00cd', '#00cdcd', '#e5e5e5',
                '#7f7f7f', '#ff0000', '#00ff00', '#ffff00', '#5c5cff', '#ff00ff', '#00ffff', '#ffffff' }
local UNDERLINES = { 'underline', 'underdouble', 'undercurl', 'underdotted', 'underdashed' }

local function palette(n)
  if n < 16 then
    return vim.g['terminal_color_' .. n] or XTERM[n + 1]
  elseif n < 232 then
    local steps, i = { 0, 95, 135, 175, 215, 255 }, n - 16
    return ('#%02x%02x%02x'):format(steps[math.floor(i / 36) + 1], steps[math.floor(i / 6) % 6 + 1], steps[i % 6 + 1])
  end
  local v = 8 + 10 * (n - 232)
  return ('#%02x%02x%02x'):format(v, v, v)
end

-- vim-view.c's colours: -1 is the default, then 0-255, then 0x1000000 | rgb.
local function colour(c)
  if c < 0 then
    return nil
  end
  return c < 256 and palette(c) or ('#%06x'):format(c - 0x1000000)
end

local groups = {} -- Highlight group names by vim-view.c's attribute ids.

local function group(id)
  local a = C.vv_attribute(id)
  local flags, style = a.flags, bit.band(bit.rshift(a.flags, 4), 7)
  local hl = { fg = colour(a.fg), bg = colour(a.bg), sp = colour(a.sp), bold = bit.band(flags, 1) ~= 0,
               italic = bit.band(flags, 2) ~= 0, reverse = bit.band(flags, 4) ~= 0,
               strikethrough = bit.band(flags, 8) ~= 0 }
  if style > 0 then
    hl[UNDERLINES[style]] = true
  end
  groups[id] = 'VimView' .. id
  api.nvim_set_hl(0, groups[id], hl)
  return groups[id]
end

-- A colour scheme clears every highlight group.
api.nvim_create_autocmd('ColorScheme', {
  callback = function()
    groups = {}
  end,
})

api.nvim_set_decoration_provider(ns, {
  on_win = function(_, _, buf)
    return buf == view.buf
  end,
  on_line = function(_, _, buf, row)
    local spans = spans_out
    for i = 0, C.vv_spans(row, spans) - 1 do
      local s = spans[0][i]
      local hl = groups[s.attr] or group(s.attr)
      if s.stop == FILL then
        api.nvim_buf_set_extmark(buf, ns, row, s.start, { end_row = row + 1, end_col = 0, hl_group = hl, hl_eol = true,
                                                          ephemeral = true, strict = false })
      else
        api.nvim_buf_set_extmark(buf, ns, row, s.start, { end_col = s.stop, hl_group = hl, ephemeral = true,
                                                          strict = false })
      end
    end
  end,
})

-- The buffer -----------------------------------------------------------------

-- vim-view.c's lines from first up to before stop.
local function texts(first, stop)
  local lines = {}
  for i = first, stop - 1 do
    lines[#lines + 1] = ffi.string(C.vv_text(i, size), size[0])
  end
  return lines
end

local function set_lines(first, last, lines)
  vim.bo[view.buf].modifiable = true
  api.nvim_buf_set_lines(view.buf, first, last, false, lines)
  vim.bo[view.buf].modifiable = false
end

local function window_options(win)
  local o = vim.wo[win]
  -- Lines keep the pane's layout. One the program filled to the pane's edge
  -- is cut off beside the gutter, as in tmux copy mode with line numbers.
  o.number, o.relativenumber, o.wrap, o.sidescrolloff = true, true, false, 0
  o.list, o.spell, o.signcolumn, o.foldcolumn, o.colorcolumn = false, false, 'no', '0', ''
  o.statuscolumn = ''
end

local function buffer()
  if view.buf and api.nvim_buf_is_valid(view.buf) then
    return view.buf
  end
  local buf = api.nvim_create_buf(false, true)
  local bo = vim.bo[buf]
  -- No file, no swap file, and no undo history for 50,000 lines. tmux keeps
  -- tab stops every 8 cells.
  bo.buftype, bo.bufhidden, bo.swapfile, bo.undolevels = 'nofile', 'hide', false, -1
  bo.modifiable, bo.tabstop = false, 8
  api.nvim_buf_set_name(buf, 'tmux scrollback')
  for _, key in ipairs({ 'q', 'i', 'a', 'I', 'A' }) do
    vim.keymap.set('n', key, function() M.leave() end, { buffer = buf, nowait = true, desc = 'Back to the tmux pane' })
  end
  -- Yanks also go to tmux's buffer; the clipboard setting covers the system.
  api.nvim_create_autocmd('TextYankPost', {
    buffer = buf,
    callback = function()
      local event = vim.v.event
      local text = table.concat(event.regcontents, '\n') .. (event.regtype == 'V' and '\n' or '')
      vim.system({ 'tmux', 'load-buffer', '-' }, { stdin = text })
    end,
  })
  view.buf = buf
  -- Last, so FileType autocommands see the finished buffer.
  bo.filetype = 'vim-view'
  return buf
end

-- Entering and leaving -------------------------------------------------------

local INFO = '#{pane_width} #{pane_height} #{cursor_x} #{cursor_y} #{history_size} #{history_collected} '
             .. '#{pane_pipe} #{pane_current_path}'

-- Entry reads the screen and this many history rows; the rest loads after.
local FIRST_ROWS = 1000

-- Rows of history coloured per command.
local COLOUR_ROWS = 4000

-- Colour the history lines before line stop, which starts at row, newest
-- first. Each line was one row or more of the captured history.
local function colour_history(stop, row)
  local generation = view.generation
  if stop <= 0 then
    log('history coloured')
    return
  end
  local first = tonumber(C.vv_chunk(stop, COLOUR_ROWS, number_out))
  local start = row - tonumber(number_out[0])
  tmux({ 'capture-pane', '-p', '-e', '-J', '-t', view.pane, '-S', ('#{e|-|:%d,#{history_size}}'):format(start),
         '-E', ('#{e|-|:%d,#{history_size}}'):format(row - 1) }, function(ok, out)
    if view.generation ~= generation or not ok then
      return
    end
    C.vv_colour(out, #out, first, stop)
    local win = vim.fn.bufwinid(view.buf)
    if win ~= -1 and vim.fn.line('w0', win) <= stop and vim.fn.line('w$', win) > first then
      api.nvim__redraw({ win = win, valid = false })
    end
    colour_history(first, start)
  end)
end

-- Insert the history above the first rows read: its text, then its colours
-- in pieces. tmux runs a client's commands back to back and redraws after
-- them, and colours cost it six times the text: one capture of 50,000
-- coloured rows would hold every redraw for 85 ms. The history's last line
-- can be the start of one the first read continued, if a wrapped line
-- crossed that row.
local function insert_history(out, last_row)
  local added = tonumber(C.vv_parse(out, #out, 0, 1))
  set_lines(0, 0, texts(0, added))
  view.first = view.first + added
  view.entry[1] = view.entry[1] + added
  log('history loaded:', added, 'lines')
  colour_history(added, last_row + 1)
end

local function load_history(last_row, wait)
  local generation = view.generation
  local capture = { 'capture-pane', '-p', '-J', '-t', view.pane, '-S', '-',
                    '-E', ('#{e|-|:%d,#{history_size}}'):format(last_row) }
  if wait then
    local ok, out = tmux(capture)
    if ok then
      insert_history(out, last_row)
    end
    return
  end
  tmux(capture, function(ok, out)
    if view.generation == generation and ok then
      insert_history(out, last_row)
    end
  end)
end

local updating, stale = false, false

-- With restore, read the pane again at a new size and keep the place: the
-- cursor's line and column, the top line, or following the last line.
function M.enter(target, float, restore)
  local started = uv.hrtime()
  view.generation, updating, stale = view.generation + 1, false, false
  view.pane, view.float, view.ready, view.waiting = target, float, false, false
  -- Tmux navigation and anything else run from here act on the floating pane.
  vim.env.TMUX_PANE = float
  -- pipe-pane shows the program's output, unless the pane already has a pipe:
  -- tmux keeps one, and that one stays.
  local ok, out = tmux({ 'if-shell', '-F', '-t', target, '#{pane_in_mode}', 'send-keys -t ' .. target .. ' -X cancel', ';',
                         'set-option', '-p', '-t', target, '@vim-view-float', float, ';',
                         'set-option', '-p', '-t', float, '@vim-view-target', target, ';',
                         'display-message', '-p', '-t', target, INFO, ';',
                         'if-shell', '-F', '-t', target, '#{?pane_pipe,0,1}',
                         ("pipe-pane -O -t %s \"exec cat > '%s'\""):format(target, fifo_path), ';',
                         'capture-pane', '-p', '-e', '-J', '-S', tostring(-FIRST_ROWS), '-E', '-', '-t', target })
  local newline = out:find('\n') or #out
  local width, height, cx, cy, history, collected, piped, path =
    out:sub(1, newline):match('^(%d+) (%d+) (%d+) (%d+) (%d+) (%d+) (%d) (.*)\n')
  if not ok or not width then
    log('entry failed:', out:sub(1, 200))
    return M.leave()
  end
  if not restore then
    view.piped = piped == '0'
  end
  width, height, cx, cy = tonumber(width), tonumber(height), tonumber(cx), tonumber(cy)
  history, collected = tonumber(history), tonumber(collected)
  -- Paths in the output, as gf follows them, are the program's.
  pcall(vim.fn.chdir, path)
  log('captured after', (uv.hrtime() - started) / 1e6, 'ms; size', width, height, 'nvim', vim.o.columns, vim.o.lines)
  if not restore then
    -- The floating pane has the pane's size; its UI gives Neovim that size.
    vim.wait(200, function()
      return vim.o.columns == width and vim.o.lines == height
    end, 2)
  end

  -- The gutter Neovim will draw: the line count's digits and a space.
  view.width, view.height, view.collected = width, height, collected
  view.text_width = width - math.max(vim.o.numberwidth - 1, #tostring(history + height)) - 1
  C.vv_clear(width, view.text_width)
  local count = tonumber(C.vv_parse(ffi.cast('const char *', out) + newline, #out - newline, 0, 0))
  local buf = buffer()
  set_lines(0, -1, texts(0, count))
  view.first = tonumber(C.vv_tail(0, height, history + height, number_out))
  view.anchor = tonumber(number_out[0])
  api.nvim_win_set_buf(0, buf)
  window_options(0)
  view.entry = { 1, 0 }

  if restore and not restore.follow then
    -- tmux wraps the history again at the new width, but its lines stay the
    -- same, so the place is the same line from the top.
    if history > FIRST_ROWS then
      load_history(history - FIRST_ROWS - 1, true)
    end
    local line = math.min(restore.line, api.nvim_buf_line_count(buf))
    vim.fn.winrestview({ topline = math.min(restore.top, line), lnum = line, col = restore.col, leftcol = 0 })
  else
    -- The pane's cursor, or the last line when following.
    local line = tonumber(C.vv_locate(view.first, view.anchor, history + cy, number_out)) + 1
    local column = tonumber(number_out[0]) + cx + 1
    if restore then
      line, column = count, 1
    end
    api.nvim_win_set_cursor(0, { line, 0 })
    local byte = vim.fn.virtcol2col(0, line, column)
    -- Lines don't wrap, so the last line goes at the bottom, as in the pane.
    local top = math.max(1, count - api.nvim_win_get_height(0) + 1)
    vim.fn.winrestview({ topline = math.min(top, line), lnum = line, col = math.max(0, byte - 1), leftcol = 0 })
  end
  -- History loading and following output move it too; they keep this current.
  view.entry = api.nvim_win_get_cursor(0)

  -- Show the pane exactly as it was. Like tmux copy mode with centering
  -- bindings, the first movement centres the cursor, not the entry itself.
  local centered = package.loaded['stay-centered']
  if centered and not restore then
    centered.disable()
    api.nvim_create_autocmd('CursorMoved', {
      buffer = buf,
      callback = function()
        local now = api.nvim_win_get_cursor(0)
        if now[1] == view.entry[1] and now[2] == view.entry[2] then
          return
        end
        centered.enable()
        vim.cmd('normal! zz')
        return true
      end,
    })
  end
  vim.cmd('redraw')
  view.ready = true
  log('shown after', (uv.hrtime() - started) / 1e6, 'ms; first', view.first, 'anchor', view.anchor, 'lines',
      api.nvim_buf_line_count(buf), restore and 'again' or '')

  -- tmux runs a client's commands back to back and redraws after them: let
  -- it draw the view before a long history capture.
  local generation = view.generation
  vim.defer_fn(function()
    if history > FIRST_ROWS and view.generation == generation and not (restore and not restore.follow) then
      load_history(history - FIRST_ROWS - 1)
    end
  end, 10)
end

-- The launcher's hook keeps the floating pane on its pane. At a new size,
-- tmux wraps the pane's rows again, so read it again; a zoomed float has a
-- new size of its own, and nothing to read.
local function reread()
  if not view.ready then
    return
  end
  local ok, size = tmux({ 'display-message', '-p', '-t', view.pane, '#{pane_width} #{pane_height}' })
  local width, height = size:match('^(%d+) (%d+)')
  if not ok or (tonumber(width) == view.width and tonumber(height) == view.height) then
    return
  end
  local cursor = api.nvim_win_get_cursor(0)
  M.enter(view.pane, view.float, { line = cursor[1], col = cursor[2], top = vim.fn.line('w0'),
                                   follow = cursor[1] == api.nvim_buf_line_count(view.buf) })
end

-- Dragging a border resizes many times; read once it settles.
local resized = uv.new_timer()
api.nvim_create_autocmd('VimResized', {
  callback = function()
    resized:start(50, 0, vim.schedule_wrap(reread))
  end,
})

-- q, i, a, I and A. tmux closes the floating pane when its UI goes, and
-- focus returns to the pane viewed.
function M.leave()
  if not pcall(vim.cmd.detach) and view.chan then
    pcall(vim.fn.chanclose, view.chan)
  end
end

-- The pool -------------------------------------------------------------------

-- See the vim-view launcher: idle/NAME holds a waiting Neovim's process id,
-- busy/NAME the panes of the view that claimed it.

local function read_file(path)
  local f = io.open(path)
  if not f then
    return nil
  end
  local text = f:read('*a')
  f:close()
  return text
end

local function alive(entry)
  local pid = tonumber(read_file(pool .. '/idle/' .. entry) or '')
  return pid ~= nil and uv.kill(pid, 0) == 0
end

local function quit()
  log('quitting')
  os.remove(pool .. '/idle/' .. name)
  os.remove(pool .. '/busy/' .. name)
  vim.cmd('qa!')
end

-- Wait for a view, unless another Neovim already does: one is enough. Of
-- spares that started together, the oldest stays.
local function wait_for_view(returning)
  for entry in vim.fs.dir(pool .. '/idle') do
    if entry ~= name and (returning or entry < name) and alive(entry) then
      return quit()
    end
  end
  local temporary = pool .. '/.' .. name
  local f = io.open(temporary, 'w')
  f:write(tostring(uv.os_getpid()))
  f:close()
  os.rename(temporary, pool .. '/idle/' .. name)
  log('waiting')
end

-- The view ended: its UI detached, or tmux closed the floating pane.
local function finish()
  local target = view.pane
  view.chan, view.pane, view.float, view.ready = nil, nil, nil, false
  view.generation = view.generation + 1
  if target then
    tmux({ 'set-option', '-pu', '-t', target, '@vim-view-float', ';', 'set-option', '-pu', '-t', target, '@vim-view-geometry',
           unpack(view.piped and { ';', 'pipe-pane', '-t', target } or {}) })
  end
  view.piped = nil
  if package.loaded['stay-centered'] then
    package.loaded['stay-centered'].enable()
  end
  C.vv_clear(view.width or 80, view.text_width or 80)
  set_lines(0, -1, {})
  os.remove(pool .. '/busy/' .. name)
  wait_for_view(true)
end

api.nvim_create_autocmd('UIEnter', {
  callback = function()
    local chan = vim.v.event.chan
    local target, float = (read_file(pool .. '/busy/' .. name) or ''):match('^(%%%d+) (%%%d+)')
    if not target or view.chan then
      return
    end
    view.chan = chan
    vim.schedule(function()
      -- The floating pane can close before this runs.
      for _, ui in ipairs(api.nvim_list_uis()) do
        if ui.chan == chan and view.chan == chan then
          return M.enter(target, float)
        end
      end
    end)
  end,
})

api.nvim_create_autocmd('UILeave', {
  callback = function()
    if view.chan and vim.v.event.chan == view.chan then
      vim.schedule(finish)
    end
  end,
})

-- Live output ----------------------------------------------------------------

-- New output reads the pane at once, then at most every INTERVAL ms.
local INTERVAL = 40
local pending, last = false, 0
local timer = uv.new_timer()

local function schedule_update()
  if not pending then
    pending = true
    timer:start(math.max(0, last + INTERVAL - uv.now()), 0, vim.schedule_wrap(function()
      pending = false
      M.update()
    end))
  end
end

-- An update re-reads the lines from the tail's first; while none of them is
-- on screen, it waits until one is.
local function tail_shown()
  local win = vim.fn.bufwinid(view.buf)
  return win ~= -1 and vim.fn.line('w$', win) > view.first
end

api.nvim_create_autocmd('WinScrolled', {
  callback = function()
    if view.waiting and view.ready and tail_shown() then
      view.waiting = false
      schedule_update()
    end
  end,
})

-- Re-read the tail, from the row where the screen started at entry.
function M.update()
  if not view.pane then
    return
  end
  if not tail_shown() then
    view.waiting = true
    return
  end
  if updating then
    stale = true
    return
  end
  updating, last = true, uv.now()
  local generation = view.generation
  tmux({ 'display-message', '-p', '-t', view.pane, '#{history_size} #{history_collected}', ';',
         'capture-pane', '-p', '-e', '-J', '-t', view.pane, '-E', '-',
         '-S', ('#{e|-|:%d,#{history_size}}'):format(view.anchor) }, function(ok, out)
    updating = false
    if stale then
      stale = false
      schedule_update()
    end
    local newline = out:find('\n')
    if view.generation ~= generation or not ok or not newline then
      return
    end
    local history, collected = out:sub(1, newline):match('^(%d+) (%d+)')
    if tonumber(collected) ~= view.collected then
      -- History was trimmed at the top; rows moved. Not handled yet.
      return
    end
    local win = vim.fn.bufwinid(view.buf)
    local last_line = api.nvim_buf_line_count(view.buf)
    local follow = win ~= -1 and api.nvim_win_get_cursor(win)[1] == last_line
    -- Only the lines that changed reach the buffer.
    local changed = tonumber(C.vv_replace(ffi.cast('const char *', out) + newline, #out - newline, view.first,
                                          number_out, end_out))
    local old_end, new_end = tonumber(number_out[0]), tonumber(end_out[0])
    if changed < old_end or changed < new_end then
      set_lines(changed, old_end, texts(changed, new_end))
    end
    local count = tonumber(C.vv_count())
    if follow and count ~= last_line then
      local entered = view.entry[1] == last_line
      api.nvim_win_set_cursor(win, { count, 0 })
      if entered then
        view.entry = api.nvim_win_get_cursor(win)
      end
    end
    -- Keep the re-read tail about one screen long.
    if count - view.first > 4 * vim.o.lines then
      view.first = tonumber(C.vv_tail(view.first, vim.o.lines, tonumber(history) + vim.o.lines, number_out))
      view.anchor = tonumber(number_out[0])
    end
  end)
end

-- The viewed program's output arrives through pipe-pane: any of it means the
-- pane changed. vim-view.c reads and drops it, without making Lua strings.
local fifo = C.vv_open_fifo(fifo_path)
uv.new_poll(fifo):start('r', function()
  if C.vv_drain(fifo) > 0 and view.pane then
    schedule_update()
  end
end)

-- Quit with the tmux server. Its socket outlives it, so watch its process.
local server = tonumber(select(2, tmux({ 'display-message', '-p', '#{pid}' })):match('%d+') or '')
uv.new_timer():start(5000, 5000, function()
  if not server or uv.kill(server, 0) ~= 0 then
    vim.schedule(function()
      vim.cmd('qa!')
    end)
  end
end)

-- The view gets the whole pane, as copy mode does.
vim.o.laststatus, vim.o.cmdheight, vim.o.showtabline = 0, 0, 0
-- Show the buffer now: its first FileType, BufEnter and BufWinEnter
-- autocommands then run while this Neovim waits, not when a view starts.
api.nvim_win_set_buf(0, buffer())
window_options(0)
api.nvim_create_autocmd('VimEnter', {
  once = true,
  callback = function()
    vim.schedule(function()
      wait_for_view(false)
    end)
  end,
})
api.nvim_create_autocmd('VimLeavePre', {
  callback = function()
    os.remove(pool .. '/idle/' .. name)
    os.remove(pool .. '/busy/' .. name)
    os.remove(fifo_path)
  end,
})
