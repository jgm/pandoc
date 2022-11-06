function ByteStringReader (input, opts)
  local chars = pandoc.List{}
  for i = 1, #input do
    chars:insert(utf8.char(input:byte(i,i)))
  end
  return pandoc.Pandoc(pandoc.Plain(pandoc.Str(table.concat(chars))))
end
