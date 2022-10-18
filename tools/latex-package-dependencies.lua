-- Print latex packages needed by pandoc's default latex template.
-- Usage: pandoc lua tools/latex-package-dependencies.lua

local templ = pandoc.template.default("latex")

local packages = {}

templ:gsub("\\usepackage *%b[] *%{([^}]*)%}", function(capt)
  capt:gsub("([^,]+)", function (pkg)
    if not pkg:find("%$") then
      packages[pkg] = true
    end
  end)
end)

templ:gsub("\\usepackage *%{([^}]*)%}", function(capt)
  capt:gsub("([^,]+)", function (pkg)
    if not pkg:find("%$") then
      packages[pkg] = true
    end
  end)
end)

for pkg,_ in pairs(packages) do
  print(pkg)
end
