local orgmode = require("orgmode")
local khalorg = require("khalorg")

khalorg.setup({
  calendar = "default",
})

orgmode.setup({
  org_agenda_files = "~/org/**/*",
  org_default_notes_file = "~/org/refile.org",
  org_custom_exports = {
    n = { label = "Add a new khal item", action = khalorg.new },
    d = { label = "Delete a khal item", action = khalorg.delete },
    e = { label = "Edit properties of a khal item", action = khalorg.edit },
    E = { label = "Edit properties & dates of a khal item", action = khalorg.edit_all },
  },
})
