local g = vim.g

local options_append = {
  netrw_keepdir = 0, --Keep the current directory and the browsing directory synced
  -- netrw_winsize = "17", -- 17% size
  netrw_localmkdir = "mkdir -p", -- change mkdir cmd
  netrw_localcopycmd = "cp -r", -- change copy command
  netrw_localrmdir = "rm -r", -- change delete command
  netrw_list_hide = [['\(^\|\s\s\)\zs\.\S\+']], -- don't show hidden
  -- netrw_banner = 0, -- hide banner
  -- netrw_liststyle = 3, -- how files are open
  -- netrw_browse_split = 0, -- open new files in vertical split
  -- netrw_altv = 1,
  netrw_bufsettings = "noma nomod nu nobl nowrap ro", -- line number
}

for k, v in pairs(options_append) do
  g[k] = v
end

local bind = function(lhs, rhs)
  vim.keymap.set("n", lhs, rhs, { remap = true })
end

-- Navigation
bind("l", "<CR>") -- open file or dir
bind(".", "gh") -- toggle dotfiles
bind("H", "u") -- preview dir
bind("h", "-^") -- go up
bind("[f", ":Explore<CR>") -- close if open

-- Marks
bind("<TAB>", "mf") -- toggle mark
bind("<S-TAB>", "mF") -- unmark
bind("<leader><TAB>", "mu") -- unmark all

-- -- Files
-- bind("ff", ":!touch ") -- create file
-- bind("fd", ":!mkdir -p ") -- create folder
-- bind("fm", ":!mv ") -- move/rename
-- bind("fc", ":!cp -r ") -- copy
-- bind("D", ":!rm -r ") -- delete
-- bind("f;", "mx") -- run command
