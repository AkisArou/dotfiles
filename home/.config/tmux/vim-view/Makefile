# Parsing for vim-view.lua, which loads it into Neovim with LuaJIT's FFI.
# The vim-view launcher runs make before it starts a Neovim, so an edited
# source is rebuilt first.
CFLAGS ?= -O2
CFLAGS += -std=c11 -Wall -Wextra

vim-view.so: vim-view.c
	$(CC) $(CFLAGS) -fPIC -shared $(LDFLAGS) -o $@.$$$$ vim-view.c && mv -f $@.$$$$ $@

# pipe-pane's command: tells a view that its pane changed.
vim-view-signal: vim-view-signal.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@.$$$$ vim-view-signal.c && mv -f $@.$$$$ $@
