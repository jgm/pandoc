return {
  {
    RawBlock = function (blk)
      local format, content = unpack(blk.c)
      if format == "markdown" then
        return pandoc.reader.markdown.read_block(content)
      else
        return blk
      end
    end,
  }
}
