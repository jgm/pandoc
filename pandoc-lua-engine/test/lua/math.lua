return {
  {
    Math = function (elem)
      if elem.mathtype == "DisplayMath" then
        elem.mathtype = "InlineMath"
      end
      return elem
    end,
  }
}
