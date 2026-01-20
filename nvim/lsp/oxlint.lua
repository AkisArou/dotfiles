return {
  cmd = { vim.fn.fnamemodify("./node_modules/.bin/" .. "oxlint", ":p"), "--lsp" },
  filetypes = {
    "javascript",
    "javascriptreact",
    "javascript.jsx",
    "typescript",
    "typescriptreact",
    "typescript.tsx",
  },
  root_dir = vim.fn.getcwd(),
  workspace_required = true,
  commands = {
    OxcFixAll = {
      function()
        local client = vim.lsp.get_clients({ bufnr = 0, name = "oxlint" })[1]
        if client == nil then
          return
        end

        ---@diagnostic disable-next-line: param-type-mismatch
        client.request("workspace/executeCommand", {
          command = "oxc.fixAll",
          arguments = {
            {
              uri = vim.uri_from_bufnr(0),
            },
          },
        }, nil, 0)
      end,
      description = "Apply fixes to current buffer using oxlint (--fix)",
    },
  },
  docs = {
    description = [[
https://oxc.rs

A collection of JavaScript tools written in Rust.

```sh
npm install [-g] oxlint
```
]],
  },
}
