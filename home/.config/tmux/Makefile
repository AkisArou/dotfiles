# The copy-mode helper. tmux.conf runs `make` on every load, so an edited
# source is rebuilt before its bindings are installed.
CFLAGS ?= -O2
CFLAGS += -std=c11 -Wall -Wextra

vim-copy: vim-copy.c vim-copy-unicode.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@.$$$$ vim-copy.c && mv -f $@.$$$$ $@

# Parsing for vim-view.lua, which loads it into Neovim with LuaJIT's FFI.
vim-view.so: vim-view.c vim-copy-unicode.h
	$(CC) $(CFLAGS) -fPIC -shared $(LDFLAGS) -o $@.$$$$ vim-view.c && mv -f $@.$$$$ $@

# Regenerate the character classes after a Python Unicode update.
unicode:
	python3 vim_copy_unicode.py

.PHONY: unicode
