local text = require 'text'

local function str_to_uppercase (s)
  return pandoc.Str(text.upper(s.text))
end

function Header (el)
  return pandoc.walk_block(el, {Str = str_to_uppercase})
end
