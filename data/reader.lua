-- A sample custom reader for a very simple markup language.
-- This parses a document into paragraphs separated by blank lines.
-- This is _{italic} and this is *{boldface}
-- This is an escaped special character: \_, \*, \{, \}
-- == text makes a level-2 heading
-- That's it!

-- For better performance we put these functions in local variables:
local P, S, R, Cf, Cc, Ct, V, Cs, Cg, Cb, B =
  lpeg.P, lpeg.S, lpeg.R, lpeg.Cf, lpeg.Cc, lpeg.Ct, lpeg.V,
  lpeg.Cs, lpeg.Cg, lpeg.Cb, lpeg.B

local whitespacechar = S(" \t\r\n")
local specialchar = S("_*{}\\")
local escapedchar = P"\\" * specialchar
         / function (x) return string.sub(x,2) end
local wordchar = (P(1) - (whitespacechar + specialchar)) + escapedchar
local spacechar = S(" \t")
local newline = P"\r"^-1 * P"\n"
local blanklines = newline * spacechar^0 * newline^1
local endline = newline - blanklines

-- Grammar
G = P{ "Pandoc",
  Pandoc = blanklines^-1 * Ct(V"Block"^0) / pandoc.Pandoc;
  Block = V"Header" + V"Para";
  Para = Ct(V"Inline"^1) * blanklines^-1 / pandoc.Para;
  Header = Ct(Cg(P("=")^1 / function(x) return #x end, "length")
             * spacechar^1
             * Cg(Ct(V"Inline"^0), "contents")
             * blanklines^-1) /
             function(res) return pandoc.Header(res.length, res.contents) end;
  Inline = V"Emph" + V"Str" + V"Space" + V"SoftBreak" + V"Special" ;
  Str = wordchar^1 / pandoc.Str;
  Space = spacechar^1 / pandoc.Space;
  SoftBreak = endline / pandoc.SoftBreak;
  Emph = Ct(P"_{" * Cg(Ct((V"Inline" - P"}")^1), "contents") * P"}") /
          function(res) return pandoc.Emph(res.contents) end;
  Special = specialchar / pandoc.Str;
}

function Reader(input)
  return lpeg.match(G, input)
end
