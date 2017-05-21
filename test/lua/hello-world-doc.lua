return {
  {
    Doc = function(doc)
      local meta = {}
      local hello = { pandoc.Str "Hello,", pandoc.Space(), pandoc.Str "World!" }
      local blocks = { pandoc.Para(hello) }
      return pandoc.Doc(blocks, meta)
    end
  }
}
