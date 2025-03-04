---@module 'blink.cmp'

local commit = {}

---@param typeAndScope string
---@param doc string
local make_completion_item = function(typeAndScope, doc)
  return {
    label = typeAndScope,
    insertText = typeAndScope,
    kind = require("blink.cmp.types").CompletionItemKind.Class,
    documentation = doc,
  }
end

local completion_items = {}

local commit_types = {
  { "feat", "A new feature for the user." },
  { "fix", "A bug fix for the user." },
  { "docs", "Documentation changes." },
  { "style", "Changes that do not affect the meaning of the code (white-space, formatting, etc.)." },
  { "refactor", "A code change that neither fixes a bug nor adds a feature." },
  { "perf", "A code change that improves performance." },
  { "test", "Adding missing tests or correcting existing tests." },
  { "chore", "Changes to the build process or auxiliary tools and libraries." },
  { "ci", "Changes to CI/CD pipelines." },
  { "revert", "Reverts a specific commit." },
}

local scopes = {
  "prm",
  "prm/back-office",
  "prm/agent",
  "volunteer",
  "volunteer/back-office",
  "volunteer/volunteer",
  "website",
  "heroui",
  "core",
}

for _, type in pairs(commit_types) do
  for _, scope in pairs(scopes) do
    table.insert(completion_items, make_completion_item(type[1] .. "(" .. scope .. ")" .. ": ", type[2]))
  end
end

local max_completion_Length = 0

for _, item in pairs(completion_items) do
  if #item.insertText > max_completion_Length then
    max_completion_Length = #item.insertText
  end
end

function commit.new(opts)
  local default_opts = {}

  opts = vim.tbl_deep_extend("keep", opts, default_opts, {
    completion_items = completion_items,
  })

  return setmetatable(opts, { __index = commit })
end

function commit:get_completions(context, callback)
  local row, col = unpack(context.cursor)
  if row ~= 1 or col > max_completion_Length then
    return -- only complete at beginning of the first line
  end

  local words = vim.split(context.line, " ")
  if #words > 1 then
    return -- only complete the first word
  end

  callback({
    is_incomplete_forward = false,
    is_incomplete_backward = false,
    items = self.completion_items,
  })

  return function() end
end

return commit
