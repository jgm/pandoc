-- filters to create the pandoc man page from MANUAL.txt

-- capitalize headers
function Header(el)
    if el.level == 1 then
      return pandoc.walk_block(el, {
        Str = function(el)
            return pandoc.Str(el.text:upper())
        end })
    end
end

function Link(el)
    return el.content
end

function Note(el)
    return {}
end
