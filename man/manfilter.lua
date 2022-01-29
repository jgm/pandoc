-- we use preloaded text to get a UTF-8 aware 'upper' function
local text = require('text')

-- capitalize level 1 headers
function Header(el)
    if el.level == 1 then
      return pandoc.walk_block(el, {
        Str = function(el)
            return pandoc.Str(text.upper(el.text))
        end })
    end
end

-- unindent table content
function Table(el)
  for _,body in ipairs(el.bodies) do
    handleTableBody(body)
  end
  return el
end

local function handleCell(el)
  if #el.contents > 0 and el.contents[1].t == "CodeBlock" then
    table.insert(el.contents, 1, pandoc.RawBlock("man", ".RS -14n"))
    table.insert(el.contents, pandoc.RawBlock("man", ".RE"))
  end
end

function handleTableBody(el)
  for _,row in ipairs(el.body) do
    for _,cell in ipairs(row.cells) do
      handleCell(cell)
    end
  end
end


-- replace links with link text
function Link(el)
    return el.content
end

-- remove notes
function Note(el)
    return {}
end
