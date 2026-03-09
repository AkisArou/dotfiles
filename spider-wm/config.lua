local actions = require("spiderwm.actions")

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
		mod = "super",
		{ { "mod", "Return" }, action = actions.spawn("/home/akisarou/dotfiles/foot/launch") },
		{ { "mod", "shift", "Return" }, action = actions.spawn("/home/akisarou/dotfiles/scripts/ssh-desktop") },
		{ { "mod", "d" }, action = actions.spawn("/home/akisarou/dotfiles/rofi/launch -show drun") },
		{ { "mod", "b" }, action = actions.spawn("/home/akisarou/dotfiles/brave/launch") },
		{ { "mod", "shift", "N" }, action = actions.spawn("/home/akisarou/dotfiles/brave/launch --incognito") },
		{ { "mod", "shift", "T" }, action = actions.spawn("/home/akisarou/dotfiles/brave/launch --incognito --tor") },
		{ { "mod", "p" }, action = actions.spawn("flameshot gui") },
		{ { "mod", "shift", "B" }, action = actions.spawn("/home/akisarou/dotfiles/rofi/bluetooth") },
		{ { "mod", "shift", "e" }, action = actions.spawn("/home/akisarou/dotfiles/rofi/emulator") },
		{ { "mod", "shift", "Q" }, action = actions.spawn("/home/akisarou/dotfiles/rofi/powermenu") },
		{ { "mod", "m" }, action = actions.spawn("/home/akisarou/dotfiles/sway/open-mail") },

		{ { "mod", "h" }, action = actions.focus_dir("left") },
		{ { "mod", "j" }, action = actions.focus_dir("down") },
		{ { "mod", "k" }, action = actions.focus_dir("up") },
		{ { "mod", "l" }, action = actions.focus_dir("right") },
		{ { "mod", "shift", "h" }, action = actions.swap_dir("left") },
		{ { "mod", "shift", "j" }, action = actions.swap_dir("down") },
		{ { "mod", "shift", "k" }, action = actions.swap_dir("up") },
		{ { "mod", "shift", "l" }, action = actions.swap_dir("right") },
		{ { "mod", "ctrl", "h" }, action = actions.resize_dir("left") },
		{ { "mod", "ctrl", "j" }, action = actions.resize_dir("down") },
		{ { "mod", "ctrl", "k" }, action = actions.resize_dir("up") },
		{ { "mod", "ctrl", "l" }, action = actions.resize_dir("right") },
		{ { "mod", "ctrl", "shift", "h" }, action = actions.resize_tiled("left") },
		{ { "mod", "ctrl", "shift", "j" }, action = actions.resize_tiled("down") },
		{ { "mod", "ctrl", "shift", "k" }, action = actions.resize_tiled("up") },
		{ { "mod", "ctrl", "shift", "l" }, action = actions.resize_tiled("right") },
		{ { "mod", "q" }, action = actions.kill_client() },
		{ { "mod", "shift", "r" }, action = actions.reload_config() },
		{ { "mod", "f" }, action = actions.toggle_fullscreen() },
		{ { "mod", "space" }, action = actions.cycle_layout() },
		{ { "mod", "shift", "space" }, action = actions.toggle_floating() },

		{ { "mod", "comma" }, action = actions.focus_mon_left() },
		{ { "mod", "period" }, action = actions.focus_mon_right() },
		{ { "mod", "shift", "comma" }, action = actions.send_mon_left() },
		{ { "mod", "shift", "period" }, action = actions.send_mon_right() },

		{ { "mod", "shift", "Left" }, action = actions.move("left") },
		{ { "mod", "shift", "Right" }, action = actions.move("right") },
		{ { "mod", "shift", "Up" }, action = actions.move("up") },
		{ { "mod", "shift", "Down" }, action = actions.move("down") },
		{ { "mod", "ctrl", "Left" }, action = actions.resize("left") },
		{ { "mod", "ctrl", "Right" }, action = actions.resize("right") },
		{ { "mod", "ctrl", "Up" }, action = actions.resize("up") },
		{ { "mod", "ctrl", "Down" }, action = actions.resize("down") },

		{ { "mod", "1" }, action = actions.view_tag(1) },
		{ { "mod", "2" }, action = actions.view_tag(2) },
		{ { "mod", "3" }, action = actions.view_tag(3) },
		{ { "mod", "4" }, action = actions.view_tag(4) },
		{ { "mod", "5" }, action = actions.view_tag(5) },
		{ { "mod", "6" }, action = actions.view_tag(6) },
		{ { "mod", "7" }, action = actions.view_tag(7) },
		{ { "mod", "8" }, action = actions.view_tag(8) },
		{ { "mod", "9" }, action = actions.view_tag(9) },

		{ { "mod", "ctrl", "1" }, action = actions.toggle_view_tag(1) },
		{ { "mod", "ctrl", "2" }, action = actions.toggle_view_tag(2) },
		{ { "mod", "ctrl", "3" }, action = actions.toggle_view_tag(3) },
		{ { "mod", "ctrl", "4" }, action = actions.toggle_view_tag(4) },
		{ { "mod", "ctrl", "5" }, action = actions.toggle_view_tag(5) },
		{ { "mod", "ctrl", "6" }, action = actions.toggle_view_tag(6) },
		{ { "mod", "ctrl", "7" }, action = actions.toggle_view_tag(7) },
		{ { "mod", "ctrl", "8" }, action = actions.toggle_view_tag(8) },
		{ { "mod", "ctrl", "9" }, action = actions.toggle_view_tag(9) },

		{ { "mod", "shift", "1" }, action = actions.tag(1) },
		{ { "mod", "shift", "2" }, action = actions.tag(2) },
		{ { "mod", "shift", "3" }, action = actions.tag(3) },
		{ { "mod", "shift", "4" }, action = actions.tag(4) },
		{ { "mod", "shift", "5" }, action = actions.tag(5) },
		{ { "mod", "shift", "6" }, action = actions.tag(6) },
		{ { "mod", "shift", "7" }, action = actions.tag(7) },
		{ { "mod", "shift", "8" }, action = actions.tag(8) },
		{ { "mod", "shift", "9" }, action = actions.tag(9) },

		{ { "mod", "ctrl", "shift", "1" }, action = actions.toggle_tag(1) },
		{ { "mod", "ctrl", "shift", "2" }, action = actions.toggle_tag(2) },
		{ { "mod", "ctrl", "shift", "3" }, action = actions.toggle_tag(3) },
		{ { "mod", "ctrl", "shift", "4" }, action = actions.toggle_tag(4) },
		{ { "mod", "ctrl", "shift", "5" }, action = actions.toggle_tag(5) },
		{ { "mod", "ctrl", "shift", "6" }, action = actions.toggle_tag(6) },
		{ { "mod", "ctrl", "shift", "7" }, action = actions.toggle_tag(7) },
		{ { "mod", "ctrl", "shift", "8" }, action = actions.toggle_tag(8) },
		{ { "mod", "ctrl", "shift", "9" }, action = actions.toggle_tag(9) },
	},
}
