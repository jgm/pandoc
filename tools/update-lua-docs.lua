local in_module_section = false

-- Generate tmp folder
local tmp_folder = os.tmpname()
os.remove(tmp_folder)
os.execute("mkdir -p " .. tmp_folder)

function extend(list1, list2)
  for i = 1, #list2 do
    list1[#list1 + 1] = list2[i]
  end
end

function module_blocks(module_filenames)
  local blocks = {}
  for _, filename in pairs(module_filenames) do
    os.execute("ldoc -q -l tools -d " .. tmp_folder .. " " .. filename)
    local module_file = io.open(tmp_folder .. "/index.html")
    local module_html = module_file:read("*a")
    local module_doc = pandoc.read(module_html, "html")
    extend(blocks, module_doc.blocks)
  end
  return blocks
end

function Header (el)
  if in_module_section then
    if el.level == 1 then
      in_module_section = false
      return el
    else
      return {}
    end
  elseif el.identifier == "module-pandoc" then
    in_module_section = true
    return module_blocks{'data/pandoc.lua'}
  elseif el.identifier == "module-pandoc.list" then
    in_module_section = true
    return module_blocks{'data/List.lua'}
  end
end

function Block (el)
  if in_module_section then
    return {}
  end
end
