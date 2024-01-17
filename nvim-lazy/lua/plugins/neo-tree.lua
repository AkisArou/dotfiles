return {
  "nvim-neo-tree/neo-tree.nvim",
  keys = {
    { "<leader>e", "<leader>fE", desc = "Explorer NeoTree (root dir)", remap = true },
    { "<leader>E", "<leader>fe", desc = "Explorer NeoTree (cwd)", remap = true },
  },
  opts = {
    window = {
      mappings = {
        ["l"] = "open",
        ["h"] = "close_node",
        ["z"] = "",
        ["<S-W>"] = "close_all_nodes",
      },
    },
    default_component_configs = {
      file_size = {
        enabled = false,
      },
    },
  },
}
