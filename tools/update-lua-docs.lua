local in_module_section = false

function pandoc_module_blocks()
  local tmp_folder = os.tmpname()
  os.remove(tmp_folder)
  os.execute("mkdir -p " .. tmp_folder)
  os.execute("ldoc -q -l tools -d " .. tmp_folder .. " data/pandoc.lua")
  local module_file = io.open(tmp_folder .. "/index.html")
  local module_html = module_file:read("*a")
  local module_doc = pandoc.read(module_html, "html")
  return module_doc.blocks
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
    return pandoc_module_blocks()
  end
end

function Block (el)
  if in_module_section then
    return {}
  end
end
