return {
  {
    SingleQuoted = function (content)
      return pandoc.Quoted("DoubleQuote", content)
    end,
  }
}
