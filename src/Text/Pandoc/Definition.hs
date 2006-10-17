-- | Definition of 'Pandoc' data structure for format-neutral representation
-- of documents.
module Text.Pandoc.Definition where

data Pandoc = Pandoc Meta [Block] deriving (Eq, Read, Show)

-- | Bibliographic information for the document:  title (list of 'Inline'),
-- authors (list of strings), date (string).
data Meta   = Meta [Inline] -- title
                   [String] -- authors
                   String   -- date
              deriving (Eq, Show, Read)

-- | Block element.
data Block  
    = Plain [Inline]        -- ^ Plain text, not a paragraph
    | Blank                 -- ^ A blank line
    | Null                  -- ^ Nothing
    | Para [Inline]         -- ^ Paragraph
    | Key [Inline] Target   -- ^ Reference key:  name (list of inlines) and 'Target'
    | CodeBlock String      -- ^ Code block (literal)
    | RawHtml String        -- ^ Raw HTML block (literal)
    | BlockQuote [Block]    -- ^ Block quote (list of blocks)
    | OrderedList [[Block]] -- ^ Ordered list (list of items, each a list of blocks)
    | BulletList [[Block]]  -- ^ Bullet list (list of items, each a list of blocks)
    | Header Int [Inline]   -- ^ Header - level (integer) and text (list of inlines)
    | HorizontalRule        -- ^ Horizontal rule
    | Note String [Block]   -- ^ Footnote or endnote - reference (string), text (list of blocks)
    deriving (Eq, Read, Show)
               
-- | Target for a link:  either a URL or an indirect (labeled) reference.
data Target 
    = Src String String     -- ^ First string is URL, second is title
    | Ref [Inline]          -- ^ Label (list of inlines) for an indirect reference
    deriving (Show, Eq, Read)

-- | Inline elements.
data Inline 
    = Str String            -- ^ Text (string)
    | Emph [Inline]         -- ^ Emphasized text (list of inlines)
    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
    | Code String           -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | LineBreak             -- ^ Hard line break
    | TeX String    -- ^ LaTeX code (literal)
    | HtmlInline String     -- ^ HTML code (literal)
    | Link [Inline] Target  -- ^ Hyperlink: text (list of inlines) and target
    | Image [Inline] Target -- ^ Image:  alternative text (list of inlines) and target
    | NoteRef String        -- ^ Footnote or endnote reference
    deriving (Show, Eq, Read)
