-- update README.md based on MANUAL.txt
-- assumes that the README.md has a div with id 'description'.
-- this gets replaced by the contents of the 'description' section
-- of the manual.

function Div(elem)
    if elem.classes[1] and elem.classes[1] == 'description' then
        local f = assert(io.open("MANUAL.txt", "r"))
        local manual = f:read("*all")
        f:close()
        local description = {}
        local i = 1
        local include = false
        local mdoc = pandoc.read(manual, "markdown")
        local blocks = mdoc.blocks
        while blocks[i] do
            if include then
                table.insert(description, pandoc.walk_block(blocks[i],
                             -- remove internal links
                             { Link = function(el)
                                 if el.target:match("^#") then
                                     return el.content
                                 end
                               end }))
            end
            if blocks[i].t == 'Header' then
                if blocks[i].identifier == 'description' then
                    include = true
                elseif include then
                    include = false
                end
            end
            i = i + 1
        end
        return pandoc.Div(description, pandoc.Attr("description",{},{}))
    end
end

