{-# OPTIONS_GHC -fglasgow-exts #-} -- for deriving Typeable
{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Definition
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definition of 'Pandoc' data structure for format-neutral representation
of documents.
-}
module Text.Pandoc.Definition where

import Data.Generics

data Pandoc = Pandoc Meta [Block] deriving (Eq, Read, Show, Typeable, Data)

-- | Bibliographic information for the document:  title (list of 'Inline'),
-- authors (list of strings), date (string).
data Meta = Meta [Inline]   -- title
                 [[Inline]] -- authors
                 [Inline]   -- date
            deriving (Eq, Show, Read, Typeable, Data)

-- | Alignment of a table column.
data Alignment = AlignLeft 
               | AlignRight 
               | AlignCenter 
               | AlignDefault deriving (Eq, Show, Read, Typeable, Data)

-- | List attributes.
type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)

-- | Style of list numbers.
data ListNumberStyle = DefaultStyle
                     | Decimal 
                     | LowerRoman 
                     | UpperRoman
                     | LowerAlpha 
                     | UpperAlpha deriving (Eq, Show, Read, Typeable, Data)

-- | Delimiter of list numbers.
data ListNumberDelim = DefaultDelim
                     | Period
                     | OneParen 
                     | TwoParens deriving (Eq, Show, Read, Typeable, Data)

-- | Attributes: identifier, classes, key-value pairs
type Attr = (String, [String], [(String, String)])

-- | Block element.
data Block  
    = Plain [Inline]        -- ^ Plain text, not a paragraph
    | Para [Inline]         -- ^ Paragraph
    | CodeBlock Attr String -- ^ Code block (literal) with attributes 
    | RawHtml String        -- ^ Raw HTML block (literal)
    | BlockQuote [Block]    -- ^ Block quote (list of blocks)
    | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
                            -- and a list of items, each a list of blocks)
    | BulletList [[Block]]  -- ^ Bullet list (list of items, each
                            -- a list of blocks)
    | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list 
                            -- Each list item is a pair consisting of a
                            -- term (a list of inlines) and one or more
                            -- definitions (each a list of blocks)
    | Header Int [Inline]   -- ^ Header - level (integer) and text (inlines) 
    | HorizontalRule        -- ^ Horizontal rule
    | Table [Inline] [Alignment] [Double] [[Block]] [[[Block]]]  -- ^ Table,
                            -- with caption, column alignments,
                            -- relative column widths (0 = default),
                            -- column headers (each a list of blocks), and
                            -- rows (each a list of lists of blocks)
    | Null                  -- ^ Nothing
    deriving (Eq, Read, Show, Typeable, Data)

-- | Type of quotation marks to use in Quoted inline.
data QuoteType = SingleQuote | DoubleQuote deriving (Show, Eq, Read, Typeable, Data)

-- | Link target (URL, title).
type Target = (String, String)

-- | Type of math element (display or inline).
data MathType = DisplayMath | InlineMath deriving (Show, Eq, Read, Typeable, Data)

-- | Inline elements.
data Inline 
    = Str String            -- ^ Text (string)
    | Emph [Inline]         -- ^ Emphasized text (list of inlines)
    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
    | Cite [Target] [Inline] -- ^ Citation (list of inlines)
    | Code String           -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | EmDash                -- ^ Em dash
    | EnDash                -- ^ En dash
    | Apostrophe            -- ^ Apostrophe
    | Ellipses              -- ^ Ellipses
    | LineBreak             -- ^ Hard line break
    | Math MathType String  -- ^ TeX math (literal)
    | TeX String            -- ^ LaTeX code (literal)
    | HtmlInline String     -- ^ HTML code (literal)
    | Link [Inline] Target  -- ^ Hyperlink: text (list of inlines), target
    | Image [Inline] Target -- ^ Image:  alt text (list of inlines), target
                            -- and target
    | Note [Block]          -- ^ Footnote or endnote 
    deriving (Show, Eq, Read, Typeable, Data)

-- | Applies a transformation on @a@s to matching elements in a @b@.
processWith :: (Data a, Data b) => (a -> a) -> b -> b
processWith f = everywhere (mkT f)

-- | Like 'processWith', but with monadic transformations.
processWithM :: (Monad m, Data a, Data b) => (a -> m a) -> b -> m b
processWithM f = everywhereM (mkM f)

-- | Runs a query on matching @a@ elements in a @c@.
queryWith :: (Data a, Data c) => (a -> [b]) -> c -> [b]
queryWith f = everything (++) ([] `mkQ` f)

{-# DEPRECATED processPandoc "Use processWith instead" #-}
processPandoc :: Data a => (a -> a) -> Pandoc -> Pandoc
processPandoc = processWith

{-# DEPRECATED queryPandoc "Use queryWith instead" #-}
queryPandoc :: Data a => (a -> [b]) -> Pandoc -> [b]
queryPandoc = queryWith

