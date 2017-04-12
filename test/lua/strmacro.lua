return {
  {
    Str = function (str)
      if str == "{{helloworld}}" then
        return pandoc.Emph {pandoc.Str "Hello, World"}
      else
        return pandoc.Str(str)
      end
    end,
  }
}
