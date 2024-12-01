-- we use preloaded text to get a UTF-8 aware 'upper' function
local text = require('text')

local manuals = {
    ["MANUAL.txt"] = { title = "pandoc", section = "1" },
    ["templates.md"] = { title = "pandoc-templates", section = "5" }
}

-- capitalize level 1 headers
function Header(el)
    if el.level == 1 then
      return pandoc.walk_block(el, {
        Str = function(el)
            return pandoc.Str(text.upper(el.text))
        end })
    end
end

-- For portability with mandoc, which doesn't allow man commands
-- inside table cells, we convert all tables to code blocks.
function Table(el)
  local rendered = pandoc.write(pandoc.Pandoc({el}), "plain")
  local adjusted = rendered  -- tame grid table lines
                     :gsub("%+([=:][=:]+)",
                       function(s)
                         return " " .. string.rep("-", #s - 1)
                       end)
                     :gsub("(%+[-:][-:]+)",
                       function(s)
                         return ""
                       end)
                     :gsub("%+\n","\n")
                     :gsub("\n|    ","\n|")
                     :gsub("|","")
  return { pandoc.RawBlock("man", ".RS -14n"),
           pandoc.CodeBlock(adjusted),
           pandoc.RawBlock("man", ".RE") }
end


-- replace links with link text
function Link(el)
    local manual = manuals[el.target]
    if manual then
        return pandoc.RawInline("man", "\\fI" .. manual.title .. "\\fR(" .. manual.section .. ")")
    else
        return el.content
    end
end

-- remove notes
function Note(el)
    return {}
end
