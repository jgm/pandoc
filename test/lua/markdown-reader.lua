return {
  {
    RawBlock = function (elem)
      if elem.format == "markdown" then
        return pandoc.reader.markdown.read_block(elem.text)
      else
        return elem
      end
    end,
  }
}
