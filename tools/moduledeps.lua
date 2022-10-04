-- Construct module dependency tree from modules.csv

local dependencies = {}

local csv = io.open("modules.csv")
local lines = csv:lines()
local mode = arg[1]
local roots = {}
local i = 2
while i <= #arg do
  roots[i - 1] = arg[i]
  i = i + 1
end

if not (mode == "tree" or mode == "transitive") then
  io.write("Usage: lua moduledeps (tree|transitive) modulename\n")
  io.exit(1)
end

if #roots == 0 then
  io.write("Usage: lua moduledeps modulename+\n")
  io.exit(1)
end

for line in lines do
  local _,_,mod,dep = string.find(line, "([^,]+),([^,]+)")
  if not dependencies[mod] then
    dependencies[mod] = {}
  end
  if not dependencies[dep] then
    dependencies[dep] = {}
  end
  dependencies[mod][dep] = true
end

local transitive = {}

function prind(ind, s)
  io.write(string.rep(" ",ind) .. s .. "\n")
end

function add_transitive_deps(mod)
  if transitive[mod] then
    return
  end
  transitive[mod] = {}
  for dep,_ in pairs(dependencies[mod]) do
    transitive[mod][dep] = true
    add_transitive_deps(dep)
    for indirectdep,_ in pairs(transitive[dep]) do
      transitive[mod][indirectdep] = true
    end
  end
end

function print_direct_deps(mod, ind)
  ind = ind or 0
  prind(ind, mod)
  for dep,_ in pairs(dependencies[mod]) do
    print_direct_deps(dep, ind + 2)
  end
end

local seen = {}
for _,root in ipairs(roots) do
  if mode == "transitive" then
      add_transitive_deps(root)
      for dep,_ in pairs(transitive[root]) do
        if not seen[dep] then
          prind(2,dep)
          seen[dep] = true
        end
      end
  elseif mode == "tree" then
    print_direct_deps(root, 0)
  end
end
