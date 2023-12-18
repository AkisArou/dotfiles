local util = {}
function util.get_npm_path()
  local npm_prefix = vim.api.nvim_exec("!npm config get prefix", true)

  local pattern = "/[%w/.-]+"

  local result = string.match(npm_prefix, pattern)
  return result
end

function util.get_css_variables_language_server_path()
  return util.get_npm_path() .. "/lib/node_modules/css-variables-language-server/dist/index.js"
end

return util
