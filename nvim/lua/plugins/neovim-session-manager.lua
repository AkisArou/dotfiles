return {
  "Shatur/neovim-session-manager",
  config = function()
    local config = require("session_manager.config")
    require("session_manager").setup({
      autoload_mode = config.AutoloadMode.CurrentDir, -- Define what to do when Neovim is started without arguments. Possible values: Disabled, CurrentDir, LastSession
    })
  end,
  dependencies = { "nvim-lua/plenary.nvim" },
}
