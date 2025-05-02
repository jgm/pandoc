std = "lua54"

max_line_length = false

files["**/test/lua/**"] = {
  std = "+pandoc_filter"
}

files["**/test/*.lua"] = {
  std = "+pandoc_custom"
}

files["data/**"] = {
  std = "+pandoc_custom"
}

files["tools/**"] = {
  std = "+pandoc_custom"
}

files["man/**"] = {
  std = "+pandoc_script"
}

exclude_files = {
  ".stack-work"
}
