# Parsing for vim-view.lua, which loads it into Neovim with LuaJIT's FFI.
# The vim-view launcher runs make before it starts a Neovim, so an edited
# source is rebuilt first.
CFLAGS ?= -O2
CFLAGS += -std=c11 -Wall -Wextra

vim-view.so: vim-view.c vim-copy-unicode.h
	$(CC) $(CFLAGS) -fPIC -shared $(LDFLAGS) -o $@.$$$$ vim-view.c && mv -f $@.$$$$ $@

# Regenerate the character classes after a Python Unicode update.
unicode:
	python3 vim_copy_unicode.py

.PHONY: unicode
