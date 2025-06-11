--- @type vim.lsp.Config
return {
  cmd = { "vscode-json-language-server", "--stdio" },
  filetypes = { "json", "jsonc" },
  root_markers = { ".git" },
  init_options = {
    provideFormatter = true,
  },
  settings = {
    json = {
      schemas = require("schemastore").json.schemas({
        extra = {
          {
            description = "EAS expo",
            fileMatch = "eas.json",
            name = "eas.json",
            url = "https://raw.githubusercontent.com/expo/eas-cli/main/packages/eas-json/schema/eas.schema.json",
          },
        },
      }),
      format = {
        enable = true,
      },
      validate = { enable = true },
    },
  },
}
