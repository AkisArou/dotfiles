require("yesterfile").setup({
  keymap = "<leader>fh",
  integrations = { codediff = true },
  picker = {
    winopts = { preview = { layout = "flex" } },
  },
})
