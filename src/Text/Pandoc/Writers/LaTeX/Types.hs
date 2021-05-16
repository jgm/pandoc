module Text.Pandoc.Writers.LaTeX.Types
  ( LW
  , WriterState (..)
  , startingState
  ) where

import Control.Monad.State.Strict (StateT)
import Data.Text (Text)
import Text.DocLayout (Doc)
import Text.Pandoc.Options
  ( WriterOptions (writerIncremental, writerTopLevelDivision)
  , TopLevelDivision (..)
  )

-- | LaTeX writer type. The type constructor @m@ will typically be an
-- instance of PandocMonad.
type LW m = StateT WriterState m

data WriterState =
  WriterState
  { stInNote        :: Bool          -- ^ true if we're in a note
  , stInQuote       :: Bool          -- ^ true if in a blockquote
  , stExternalNotes :: Bool          -- ^ true if in context where
                                     --   we need to store footnotes
  , stInMinipage    :: Bool          -- ^ true if in minipage
  , stInHeading     :: Bool          -- ^ true if in a section heading
  , stInItem        :: Bool          -- ^ true if in \item[..]
  , stNotes         :: [Doc Text]    -- ^ notes in a minipage
  , stOLLevel       :: Int           -- ^ level of ordered list nesting
  , stOptions       :: WriterOptions -- ^ writer options, so they don't have to
                                     --   be parameter
  , stVerbInNote    :: Bool          -- ^ true if document has verbatim text in note
  , stTable         :: Bool          -- ^ true if document has a table
  , stMultiRow      :: Bool          -- ^ true if document has multirow cells
  , stStrikeout     :: Bool          -- ^ true if document has strikeout
  , stUrl           :: Bool          -- ^ true if document has visible URL link
  , stGraphics      :: Bool          -- ^ true if document contains images
  , stLHS           :: Bool          -- ^ true if document has literate haskell code
  , stHasChapters   :: Bool          -- ^ true if document has chapters
  , stCsquotes      :: Bool          -- ^ true if document uses csquotes
  , stHighlighting  :: Bool          -- ^ true if document has highlighted code
  , stIncremental   :: Bool          -- ^ true if beamer lists should be
  , stZwnj          :: Bool          -- ^ true if document has a ZWNJ character
  , stInternalLinks :: [Text]        -- ^ list of internal link targets
  , stBeamer        :: Bool          -- ^ produce beamer
  , stEmptyLine     :: Bool          -- ^ true if no content on line
  , stHasCslRefs    :: Bool          -- ^ has a Div with class refs
  , stIsFirstInDefinition :: Bool    -- ^ first block in a defn list
  }

startingState :: WriterOptions -> WriterState
startingState options =
  WriterState
  { stInNote = False
  , stInQuote = False
  , stExternalNotes = False
  , stInHeading = False
  , stInMinipage = False
  , stInItem = False
  , stNotes = []
  , stOLLevel = 1
  , stOptions = options
  , stVerbInNote = False
  , stTable = False
  , stMultiRow = False
  , stStrikeout = False
  , stUrl = False
  , stGraphics = False
  , stLHS = False
  , stHasChapters = case writerTopLevelDivision options of
                      TopLevelPart    -> True
                      TopLevelChapter -> True
                      _               -> False
  , stCsquotes = False
  , stHighlighting = False
  , stIncremental = writerIncremental options
  , stZwnj = False
  , stInternalLinks = []
  , stBeamer = False
  , stEmptyLine = True
  , stHasCslRefs = False
  , stIsFirstInDefinition = False
  }
