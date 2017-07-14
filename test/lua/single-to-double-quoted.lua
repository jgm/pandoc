return {
  {
    Quoted = function (elem)
      if elem.quotetype == "SingleQuote" then
        elem.quotetype = "DoubleQuote"
      end
      return elem
    end,
  }
}
