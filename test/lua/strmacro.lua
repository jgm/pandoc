return {
  { Str = function (inline)
      if inline.c == "{{helloworld}}" then
        return pandoc.Emph {pandoc.Str "Hello, World"}
      else
        return inline
      end
  end,
  }
}
