return {
  {
    Note = function (elem)
      if elem.notetype == "Footnote" then
        elem.notetype = "Endnote"
      end
      return elem
    end,
  }
}
