-- A sample custom reader for a very simple markup language.
-- This parses a document into paragraphs separated by blank lines.
-- This is /italic/ and this is *boldface* and this is `code`
-- and `code``with backtick` (doubled `` = ` inside backticks).
-- This is an escaped special character: \_, \*, \\
-- == text makes a level-2 heading
-- That's it!

-- For better performance we put these functions in local variables:
local P, S, R, Cf, Cc, Ct, V, Cs, Cg, Cb, B, C, Cmt =
  lpeg.P, lpeg.S, lpeg.R, lpeg.Cf, lpeg.Cc, lpeg.Ct, lpeg.V,
  lpeg.Cs, lpeg.Cg, lpeg.Cb, lpeg.B, lpeg.C, lpeg.Cmt

--- if item is a table, concatenate it to acc;
-- otherwise insert it at the end.
local function add_item(acc, item)
  if acc == nil then
    acc = {}
  end
  if type(item) == "table" then
    for i = 1,#item do
      add_item(acc, item[i])
    end
  else
    acc[#acc + 1] = item
  end
  return acc
end

local function Many1(parser)
  return Cf(Cc(nil) * parser^1 , add_item)
end

local function Many(parser)
  return (Many1(parser) + Cc{})
end

local whitespacechar = S(" \t\r\n")
local specialchar = S("/*\\`")
local wordchar = (1 - (whitespacechar + specialchar))
local spacechar = S(" \t")
local newline = P"\r"^-1 * P"\n"
local blanklines = newline * (spacechar^0 * newline)^1
local endline = newline - blanklines

local function BetweenDelims(c, parser, constructor)
  local starter = P(c) * #(- whitespacechar)
  local ender = B(1 - whitespacechar) * P(c)
  return starter * Many(parser - ender) * C(ender^-1) /
          function(contents, ender)
            if ender == "" then -- fallback
              return { pandoc.Str(c) , contents }
            else
              return constructor(contents)
            end
          end
end


-- Grammar
G = P{ "Pandoc",
  Pandoc = Many(V"Block") / pandoc.Pandoc;
  Block = blanklines^0 * (V"Header" + V"Para") ;
  Para = Many1(V"Inline") * blanklines^-1 / pandoc.Para;
  Header = (P("=")^1 / string.len)
             * spacechar^1
             * Many(V"Inline" - (spacechar^0 * P("=")^0 * blanklines))
             * spacechar^0
             * P("=")^0
             * blanklines^-1 /
             function(lev, contents) return pandoc.Header(lev, contents) end;
  Inline = V"Emph" + V"Strong" + V"Str" + V"Space" + V"SoftBreak" +
             V"Code" + V"Escaped" + V"Special";
  Str = wordchar^1 / pandoc.Str;
  Escaped = "\\" * C(specialchar) / function(s) return pandoc.Str(s) end;
  Space = spacechar^1 / pandoc.Space;
  SoftBreak = endline / pandoc.SoftBreak;
  Emph = BetweenDelims("/", V"Inline", pandoc.Emph);
  Strong = BetweenDelims("*", V"Inline", pandoc.Strong);
  Code = P"`" * Ct(( (P"``" / "`") + (C(1) - S"`"))^1) * P"`"
         / table.concat / pandoc.Code;
  Special = S"`\\" / pandoc.Str;
}

function Reader(input, opts)
  return lpeg.match(G, input)
end
