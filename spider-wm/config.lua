local mod = "super"

---@type spwm.Config
return {
	tags = { "1", "2", "3", "4", "5", "6", "7", "8", "9" },

	options = {
		layouts_dir = os.getenv("HOME") .. "/.config/spider-wm/layouts",
		sloppyfocus = true,
		decorations = "none",
		border_width = 2,
		border_color_focused = "#285577",
		border_color_unfocused = "#222222",
	},

	outputs = {
		["Samsung Electric Company LC49G95T H4ZR303132"] = {
			mode = "5120x1440@130Hz",
		},
		["eDP-1"] = {
			scale = 1.5,
		},
	},

	inputs = {
		["type:keyboard"] = {
			xkb_layout = "us,gr",
			xkb_model = "pc105+inet",
			xkb_options = "grp:win_space_toggle",
			repeat_delay = 220,
			repeat_rate = 33,
		},
		["type:touchpad"] = {
			natural_scroll = true,
			tap = true,
			drag_lock = false,
			accel_profile = "adaptive",
		},
	},

	layouts = {
		default = "main",
		per_tag = {
			[1] = "main",
			[2] = "main",
			[3] = "main",
			[4] = "main",
			[5] = "main",
			[6] = "main",
			[7] = "main",
			[8] = "main",
			[9] = "genymotion",
		},
		per_monitor = {
			["eDP-1"] = "main",
		},
	},

	rules = {
		-- { app_id = "foot", tags = 1 },
		-- { app_id = "org.genymotion.scrcpy", tags = 9, floating = true },
		-- { app_id = "rofi", floating = true },
	},

	autostart = {
		"swaybg --output '*' --mode fill --image /home/akisarou/dotfiles/wallpapers/forest.jpg",
		"/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",
		"sh -c '$HOME/dotfiles/waybar/launch-waybar'",
		"flameshot",
		"/home/akisarou/.local/bin/emacs-preload-server",
	},

	autostart_once = {
		"dbus-update-activation-environment --all",
		"systemctl --user import-environment WAYLAND_DISPLAY DISPLAY XDG_CURRENT_DESKTOP XCURSOR_SIZE XCURSOR_THEME",
		"sh -c '$HOME/dotfiles/scripts/start-tmux'",
	},

	bindings = {
		{ mods = { mod }, key = "Return", action = "spawn", arg = "/home/akisarou/dotfiles/foot/launch" },
		{
			mods = { mod, "shift" },
			key = "Return",
			action = "spawn",
			arg = "/home/akisarou/dotfiles/scripts/ssh-desktop",
		},
		{ mods = { mod }, key = "d", action = "spawn", arg = "/home/akisarou/dotfiles/rofi/launch -show drun" },
		{ mods = { mod }, key = "b", action = "spawn", arg = "/home/akisarou/dotfiles/brave/launch" },
		{
			mods = { mod, "shift" },
			key = "N",
			action = "spawn",
			arg = "/home/akisarou/dotfiles/brave/launch --incognito",
		},
		{
			mods = { mod, "shift" },
			key = "T",
			action = "spawn",
			arg = "/home/akisarou/dotfiles/brave/launch --incognito --tor",
		},
		{ mods = { mod }, key = "p", action = "spawn", arg = "flameshot gui" },
		{ mods = { mod, "shift" }, key = "B", action = "spawn", arg = "/home/akisarou/dotfiles/rofi/bluetooth" },
		{ mods = { mod, "shift" }, key = "e", action = "spawn", arg = "/home/akisarou/dotfiles/rofi/emulator" },
		{ mods = { mod, "shift" }, key = "Q", action = "spawn", arg = "/home/akisarou/dotfiles/rofi/powermenu" },
		{ mods = { mod }, key = "m", action = "spawn", arg = "/home/akisarou/dotfiles/sway/open-mail" },

		{ mods = { mod }, key = "h", action = "focus_dir", arg = "left" },
		{ mods = { mod }, key = "j", action = "focus_dir", arg = "down" },
		{ mods = { mod }, key = "k", action = "focus_dir", arg = "up" },
		{ mods = { mod }, key = "l", action = "focus_dir", arg = "right" },
		{ mods = { mod, "shift" }, key = "h", action = "swap_dir", arg = "left" },
		{ mods = { mod, "shift" }, key = "j", action = "swap_dir", arg = "down" },
		{ mods = { mod, "shift" }, key = "k", action = "swap_dir", arg = "up" },
		{ mods = { mod, "shift" }, key = "l", action = "swap_dir", arg = "right" },
		{ mods = { mod, "ctrl" }, key = "h", action = "resize_dir", arg = "left" },
		{ mods = { mod, "ctrl" }, key = "j", action = "resize_dir", arg = "down" },
		{ mods = { mod, "ctrl" }, key = "k", action = "resize_dir", arg = "up" },
		{ mods = { mod, "ctrl" }, key = "l", action = "resize_dir", arg = "right" },
		{ mods = { mod, "ctrl", "shift" }, key = "h", action = "resize_tiled", arg = "left" },
		{ mods = { mod, "ctrl", "shift" }, key = "j", action = "resize_tiled", arg = "down" },
		{ mods = { mod, "ctrl", "shift" }, key = "k", action = "resize_tiled", arg = "up" },
		{ mods = { mod, "ctrl", "shift" }, key = "l", action = "resize_tiled", arg = "right" },
		{ mods = { mod }, key = "q", action = "kill_client" },
		{ mods = { mod, "shift" }, key = "r", action = "reload_config" },
		{ mods = { mod }, key = "f", action = "toggle_fullscreen" },
		{ mods = { mod }, key = "space", action = "cycle_layout" },
		{ mods = { mod, "shift" }, key = "space", action = "toggle_floating" },

		{ mods = { mod }, key = "comma", action = "focus_mon_left" },
		{ mods = { mod }, key = "period", action = "focus_mon_right" },
		{ mods = { mod, "shift" }, key = "comma", action = "send_mon_left" },
		{ mods = { mod, "shift" }, key = "period", action = "send_mon_right" },

		{ mods = { mod, "shift" }, key = "Left", action = "move", arg = "left" },
		{ mods = { mod, "shift" }, key = "Right", action = "move", arg = "right" },
		{ mods = { mod, "shift" }, key = "Up", action = "move", arg = "up" },
		{ mods = { mod, "shift" }, key = "Down", action = "move", arg = "down" },
		{ mods = { mod, "ctrl" }, key = "Left", action = "resize", arg = "left" },
		{ mods = { mod, "ctrl" }, key = "Right", action = "resize", arg = "right" },
		{ mods = { mod, "ctrl" }, key = "Up", action = "resize", arg = "up" },
		{ mods = { mod, "ctrl" }, key = "Down", action = "resize", arg = "down" },

		{ mods = { mod }, key = "1", action = "view_tag", arg = 1 },
		{ mods = { mod }, key = "2", action = "view_tag", arg = 2 },
		{ mods = { mod }, key = "3", action = "view_tag", arg = 3 },
		{ mods = { mod }, key = "4", action = "view_tag", arg = 4 },
		{ mods = { mod }, key = "5", action = "view_tag", arg = 5 },
		{ mods = { mod }, key = "6", action = "view_tag", arg = 6 },
		{ mods = { mod }, key = "7", action = "view_tag", arg = 7 },
		{ mods = { mod }, key = "8", action = "view_tag", arg = 8 },
		{ mods = { mod }, key = "9", action = "view_tag", arg = 9 },

		{ mods = { mod, "ctrl" }, key = "1", action = "toggle_view_tag", arg = 1 },
		{ mods = { mod, "ctrl" }, key = "2", action = "toggle_view_tag", arg = 2 },
		{ mods = { mod, "ctrl" }, key = "3", action = "toggle_view_tag", arg = 3 },
		{ mods = { mod, "ctrl" }, key = "4", action = "toggle_view_tag", arg = 4 },
		{ mods = { mod, "ctrl" }, key = "5", action = "toggle_view_tag", arg = 5 },
		{ mods = { mod, "ctrl" }, key = "6", action = "toggle_view_tag", arg = 6 },
		{ mods = { mod, "ctrl" }, key = "7", action = "toggle_view_tag", arg = 7 },
		{ mods = { mod, "ctrl" }, key = "8", action = "toggle_view_tag", arg = 8 },
		{ mods = { mod, "ctrl" }, key = "9", action = "toggle_view_tag", arg = 9 },

		{ mods = { mod, "shift" }, key = "1", action = "tag", arg = 1 },
		{ mods = { mod, "shift" }, key = "2", action = "tag", arg = 2 },
		{ mods = { mod, "shift" }, key = "3", action = "tag", arg = 3 },
		{ mods = { mod, "shift" }, key = "4", action = "tag", arg = 4 },
		{ mods = { mod, "shift" }, key = "5", action = "tag", arg = 5 },
		{ mods = { mod, "shift" }, key = "6", action = "tag", arg = 6 },
		{ mods = { mod, "shift" }, key = "7", action = "tag", arg = 7 },
		{ mods = { mod, "shift" }, key = "8", action = "tag", arg = 8 },
		{ mods = { mod, "shift" }, key = "9", action = "tag", arg = 9 },

		{ mods = { mod, "ctrl", "shift" }, key = "1", action = "toggle_tag", arg = 1 },
		{ mods = { mod, "ctrl", "shift" }, key = "2", action = "toggle_tag", arg = 2 },
		{ mods = { mod, "ctrl", "shift" }, key = "3", action = "toggle_tag", arg = 3 },
		{ mods = { mod, "ctrl", "shift" }, key = "4", action = "toggle_tag", arg = 4 },
		{ mods = { mod, "ctrl", "shift" }, key = "5", action = "toggle_tag", arg = 5 },
		{ mods = { mod, "ctrl", "shift" }, key = "6", action = "toggle_tag", arg = 6 },
		{ mods = { mod, "ctrl", "shift" }, key = "7", action = "toggle_tag", arg = 7 },
		{ mods = { mod, "ctrl", "shift" }, key = "8", action = "toggle_tag", arg = 8 },
		{ mods = { mod, "ctrl", "shift" }, key = "9", action = "toggle_tag", arg = 9 },
	},
}
