function ByteStringWriter (doc, opts)
  local buffer = {}
  for i=0, 255 do
    table.insert(buffer, string.char(i))
  end
  return table.concat(buffer, '')
end
