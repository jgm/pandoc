-- Extract changes from latest version in changelog.

function Pandoc(el)
  local newblocks = {}
  i = 1
  while i <= #el.blocks and
      not (el.blocks[i].t == "Header" and el.blocks[i].level == 2) do
    i = i+1
  end
  while i <= #el.blocks do
    i = i+1
    if el.blocks[i].t == "Header" and el.blocks[i].level == 2 then
      break
    end
    table.insert(newblocks, el.blocks[i])
  end
  return pandoc.Pandoc(newblocks)
end
