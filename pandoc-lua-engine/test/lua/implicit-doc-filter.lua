function Pandoc (doc)
  local meta = {}
  local hello = { pandoc.Str "Hello,", pandoc.Space(), pandoc.Str "World!" }
  local blocks = { pandoc.Para(hello) }
  return pandoc.Pandoc(blocks, meta)
end
