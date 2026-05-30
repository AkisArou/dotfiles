local thirdsColumns = {
	order = {},
	widths = {},
	last_active_id = nil,
	min_width = 0.1,
}

local function targetId(target)
	local window = target.window
	return window and tostring(window.stable_id) or tostring(target.index)
end

local function indexOf(items, value)
	for i, item in ipairs(items) do
		if item == value then
			return i
		end
	end
end

local function activeTargetId(ctx)
	local active_window = hl.get_active_window()
	if active_window then
		local active_id = tostring(active_window.stable_id)
		for _, target in ipairs(ctx.targets) do
			if targetId(target) == active_id then
				return active_id
			end
		end

		for _, target in ipairs(ctx.targets) do
			local window = target.window
			if window and window.address == active_window.address then
				return targetId(target)
			end
		end
	end

	for _, target in ipairs(ctx.targets) do
		local window = target.window
		if window and window.active then
			return targetId(target)
		end
	end
end

local function visualOrder()
	if #thirdsColumns.order == 2 then
		return { thirdsColumns.order[2], thirdsColumns.order[1] }
	end

	local order = {}
	for i, id in ipairs(thirdsColumns.order) do
		order[i] = id
	end

	return order
end

local function setOrderFromVisual(order)
	if #order == 2 then
		thirdsColumns.order = { order[2], order[1] }
	else
		thirdsColumns.order = order
	end
end

local function syncOrder(ctx)
	local present = {}
	local targets = {}

	for _, target in ipairs(ctx.targets) do
		local id = targetId(target)
		present[id] = true
		targets[id] = target
	end

	local active_id = activeTargetId(ctx)
	local insert_after = thirdsColumns.last_active_id
	if active_id and indexOf(thirdsColumns.order, active_id) then
		insert_after = active_id
	end

	local old_order = thirdsColumns.order
	thirdsColumns.order = {}
	for _, id in ipairs(old_order) do
		if present[id] then
			table.insert(thirdsColumns.order, id)
		else
			thirdsColumns.widths[id] = nil
		end
	end

	for _, target in ipairs(ctx.targets) do
		local id = targetId(target)
		if not indexOf(thirdsColumns.order, id) then
			local after = insert_after and indexOf(thirdsColumns.order, insert_after)
			table.insert(thirdsColumns.order, after and (after + 1) or (#thirdsColumns.order + 1), id)
			insert_after = id
		end
	end

	if active_id then
		thirdsColumns.last_active_id = active_id
	end

	return targets
end

local function clamp(value, min, max)
	return math.max(min, math.min(max, value))
end

local function defaultWidth(n)
	if n <= 2 then
		return 1 / 3
	end

	return 1 / n
end

local function fitWidths(widths)
	local total = 0
	for _, width in ipairs(widths) do
		total = total + width
	end

	if total <= 1 then
		return widths, total
	end

	for i, width in ipairs(widths) do
		widths[i] = width / total
	end

	return widths, 1
end

local function resizeActive(ctx, pixels)
	local active_id = activeTargetId(ctx)
	if not active_id or ctx.area.w == 0 then
		return
	end

	local n = #ctx.targets
	local delta = pixels / ctx.area.w
	local active_index = indexOf(thirdsColumns.order, active_id)
	local current = thirdsColumns.widths[active_id] or defaultWidth(n)
	local next_width = clamp(current + delta, thirdsColumns.min_width, 1)

	if n >= 3 and active_index then
		local neighbor_index = active_index < n and (active_index + 1) or (active_index - 1)
		local neighbor_id = thirdsColumns.order[neighbor_index]
		local neighbor_width = neighbor_id and (thirdsColumns.widths[neighbor_id] or defaultWidth(n))
		if neighbor_id and neighbor_width then
			local actual_delta = next_width - current
			local next_neighbor_width = clamp(neighbor_width - actual_delta, thirdsColumns.min_width, 1)
			actual_delta = neighbor_width - next_neighbor_width
			thirdsColumns.widths[active_id] = current + actual_delta
			thirdsColumns.widths[neighbor_id] = next_neighbor_width
			return
		end
	end

	thirdsColumns.widths[active_id] = next_width
end

hl.layout.register("thirds_columns", {
	recalculate = function(ctx)
		local n = #ctx.targets
		if n == 0 then
			return
		end

		local targets = syncOrder(ctx)
		local widths = {}
		for i, id in ipairs(thirdsColumns.order) do
			widths[i] = clamp(thirdsColumns.widths[id] or defaultWidth(n), thirdsColumns.min_width, 1)
		end

		local total_width
		widths, total_width = fitWidths(widths)
		local x = ctx.area.x + ((ctx.area.w * (1 - total_width)) / 2)
		local visual_order = visualOrder()
		if n == 2 then
			x = ctx.area.x
		end

		for _, id in ipairs(visual_order) do
			local target = targets[id]
			if target then
				local width_index = indexOf(thirdsColumns.order, id)
				local width = ctx.area.w * widths[width_index]
				target:place({
					x = x,
					y = ctx.area.y,
					w = width,
					h = ctx.area.h,
				})
				x = x + width
			end
		end
	end,

	layout_msg = function(ctx, msg)
		local command, amount = msg:match("^(%S+)%s*([%-]?%d*)")
		if command == "resize" then
			resizeActive(ctx, tonumber(amount) or 0)
		elseif command == "move" then
			local active_id = activeTargetId(ctx)
			local order = visualOrder()
			local i = active_id and indexOf(order, active_id)
			local j = i and (i + (tonumber(amount) or 0))
			if not i or j < 1 or j > #order then
				return true
			end
			order[i], order[j] = order[j], order[i]
			setOrderFromVisual(order)
		else
			return "thirds_columns: expected resize <pixels> or move <-1|1>"
		end

		return true
	end,
})
