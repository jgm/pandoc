-- Test that constructors are "smart" in that they autoconvert
-- types where sensible.
function Para (_)
  return {
    pandoc.BulletList{pandoc.Para "Hello", pandoc.Para "World"},
    pandoc.DefinitionList{{"foo", pandoc.Para "placeholder"}},
    pandoc.LineBlock{"Moin", "Welt"},
    pandoc.OrderedList{pandoc.Plain{pandoc.Str "one"}, pandoc.Plain "two"}
  }
end
