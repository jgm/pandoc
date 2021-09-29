function Doc (doc)
  local meta = {}
  local hello = { pandoc.Str "Hello, World!" }
  local blocks = { pandoc.Para(hello) }
  return pandoc.Pandoc(blocks, meta)
end
