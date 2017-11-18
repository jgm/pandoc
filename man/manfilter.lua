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

-- replace links with link text
function Link(el)
    return el.content
end

-- remove notes
function Note(el)
    return {}
end
