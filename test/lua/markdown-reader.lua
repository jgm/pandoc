return {
  {
    RawBlock = function (elem)
      if elem.format == "markdown" then
        local pd = pandoc.read(elem.text, "markdown")
        return pd.blocks[1]
      else
        return elem
      end
    end,
  }
}
