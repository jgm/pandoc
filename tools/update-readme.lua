-- update README.md based on MANUAL.txt
-- inserts contents of input-formats and output-formats

local f = assert(io.open("MANUAL.txt", "r"))
local manual = f:read("*all")
mdoc = pandoc.read(manual, "markdown")
f:close()
result = {}

function Div(elem)
  local ident = elem.identifier or ""
  local fixrel = function(el)
    if el.target:match("^#") then
      el.target = "https://pandoc.org/MANUAL.html" .. el.target
    end
    return el
  end
  local get = function(el)
    if el.identifier == ident then
      result = pandoc.walk_block(el, { Link = fixrel  })
    end
  end
  if ident == 'input-formats' or ident == 'output-formats' then
    pandoc.walk_block(pandoc.Div(mdoc.blocks), { Div = get })
    return result
  end
end

