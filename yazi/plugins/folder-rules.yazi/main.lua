local function setup()
	ps.sub("cd", function()
		local cwd = cx.active.current.cwd
		if cwd:ends_with("dotfiles") then
			ya.emit("hidden", { "show" })
		end
	end)
end

return { setup = setup }
