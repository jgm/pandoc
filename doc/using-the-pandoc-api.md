% Using the pandoc API
% John MacFarlane

Pandoc can be used as a Haskell library, to write your own
conversion tools or power a web application.  This document
offers an introduction to using the pandoc API.

Detailed API documentation at the level of individual functions
and types is available at
<https://hackage.haskell.org/package/pandoc>.

# Pandoc's architecture

Pandoc is structured as a set of *readers*, which translate
various input formats into an abstract syntax tree (the
Pandoc AST) representing a structured document, and a set of
*writers*, which render this AST into various input formats.
Pictorially:

```
[input format] ==reader==> [Pandoc AST] ==writer==> [output format]
```

This architecture allows pandoc to perform $M \times n$
conversions with $M$ readers and $N$ writers.

The Pandoc AST is defined in the
[pandoc-types](https://hackage.haskell.org/package/pandoc-types)
package.  You should start by looking at the Haddock
documentation for
[Text.Pandoc.Definition](https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html).  As you'll see, a `Pandoc` is
composed of some metadata and a list of `Block`s.  There are
various kinds of `Block`, including `Para` (paragraph),
`Header` (section heading), and `BlockQuote`.  Some of the
`Block`s (like `BlockQuote`) contain lists of `Block`s,
while others (like `Para`) contain lists of `Inline`s, and
still others (like `CodeBlock`) contain plain text or
nothing.  `Inline`s are the basic elements of paragraphs.
The distinction between `Block` and `Inline` in the type
system makes it impossible to represent, for example,
a link (`Inline`) whose link text is a block quote (`Block`).
This expressive limitation is mostly a help rather than a
hindrance, since many of the formats pandoc supports have
similar limitations.

The best way to explore the pandoc AST is to use `pandoc -t
native`, which will display the AST correspoding to some
Markdown input:

```
% echo -e "1. *foo*\n2. bar" | pandoc -t native
[OrderedList (1,Decimal,Period)
 [[Plain [Emph [Str "foo"]]]
 ,[Plain [Str "bar"]]]]
```

# A simple example

Here is a simple example of the use of a pandoc reader and
writer to perform a conversion inside ghci:

```
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  result <- runIO $ do
    doc <- readMarkdown def (T.pack "[testing](url)")
    writeRST def doc
  rst <- handleError result
  TIO.putStrLn rst
```

Some notes:

1. The first part constructs a conversion pipeline: the input
   string is passed to `readMarkdown`, and the resulting Pandoc
   AST (`doc`) is then rendered by `writeRST`.  The conversion
   pipeline is "run" by `runIO`---more on that below.

2. `result` has the type `Either PandocError Text`.  We could
   pattern-match on this manually, but it's simpler in this
   context to use the `handleError` function from
   Text.Pandoc.Error.  This exits with an appropriate error
   code and message if the value is a `Left`, and returns the
   `Text` if the value is a `Right`.

# The PandocMonad class

Let's look at the types of `readMarkdown` and `writeRST`:

```haskell
readMarkdown :: PandocMonad m => ReaderOptions -> Text -> m Pandoc

writeRST :: PandocMonad m => WriterOptions -> Pandoc -> m Text
```

The `PandocMonad m =>` part is a typeclass constraint.
It says that `readMarkdown` and `writeRST` define computations
that can be used in any instance of the `PandocMonad`
type class.  `PandocMonad` is defined in the module
Text.Pandoc.Class.

Two instances of `PandocMonad` are provided: `PandocIO` and
`PandocPure`. The difference is that computations run in
`PandocIO` are allowed to do IO (for example, read a file),
while computations in `PandocPure` are free of any side effects.
`PandocPure` is useful for sandboxed environments, when you want
to prevent users from doing anything malicious.  To run the
conversion in `PandocIO`, use `runIO` (as above).  To run it in
`PandocPure`, use `runPure`.

As you can see from the Haddocks,
[Text.Pandoc.Class](https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Class.html)
exports many auxiliary functions that can be used in any
instance of `PandocMonad`.  For example:

```haskell
-- | Get the verbosity level.
getVerbosity :: PandocMonad m => m Verbosity

-- | Set the verbosity level.
setVerbosity :: PandocMonad m => Verbosity -> m ()

-- Get the accomulated log messages (in temporal order).
getLog :: PandocMonad m => m [LogMessage]
getLog = reverse <$> getsCommonState stLog

-- | Log a message using 'logOutput'.  Note that
-- 'logOutput' is called only if the verbosity
-- level exceeds the level of the message, but
-- the message is added to the list of log messages
-- that will be retrieved by 'getLog' regardless
-- of its verbosity level.
report :: PandocMonad m => LogMessage -> m ()

-- | Fetch an image or other item from the local filesystem or the net.
-- Returns raw content and maybe mime type.
fetchItem :: PandocMonad m
          => String
          -> m (B.ByteString, Maybe MimeType)

setResourcePath :: PandocMonad m => [FilePath] -> m ()
```

If we wanted more verbose informational messages
during the conversion we defined in the previous
section, we could do this:

```haskell
  result <- runIO $ do
    setVerbosity INFO
    doc <- readMarkdown def (T.pack "[testing](url)")
    writeRST def doc
```

# Options

The first argument of each reader or writer is for
options controlling the behavior of the reader or writer:
`ReaderOptions` for readers and `WriterOptions`
for writers.  These are defined in
[Text.Pandoc.Options](https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Options.html).  It is a good idea to study these
options to see what can be adjusted.

`def` (from Data.Default) denotes a default value for
each kind of option.  (You can also use `defaultWriterOptions`
and `defaultReaderOptions`.)  Generally you'll want to use
the defaults and modify them only when needed, for example:

```haskell
    writeRST def{ writerReferenceLinks = True }
```

Some particularly important options to know about:

1.  `writerTemplate`:  By default, this is `Nothing`, which
    means that a document fragment will be produced. If you
    want a full document, you need to specify `Just template`,
    where `template` is a String containing the template's
    contents (not the path).

2.  `readerExtensions` and `writerExtensions`:  These specify
    the extensions to be used in parsing and rendering.
    Extensions are defined in [Text.Pandoc.Extensions](https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Extensions.html).

# Builder

Sometimes it's useful to construct a Pandoc document
programatically.  To make this easier we provide the
module [Text.Pandoc.Builder](https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html) in `pandoc-types`.

Because concatenating lists is slow, we use special
types `Inlines` and `Blocks` that wrap a `Sequence` of
`Inline` and `Block` elements.  These are instances
of the Monoid typeclass and can easily be concatenated:

```haskell
import Text.Pandoc.Builder

mydoc :: Pandoc
mydoc = doc $ header 1 (text "Hello!")
           <> para (emph (text "hello world") <> text ".")

main :: IO ()
main = print mydoc
```

If you use the `{-# LANGUAGE OverloadedStrings #-}`, you can
simplify this further:

```haskell
mydoc = doc $ header 1 "Hello!"
           <> para (emph "hello world" <> ".")
```

Here's a more realistic example.  Suppose your boss says: write
me a letter in Word listing all the filling stations in Chicago
that take the Voyager card.  You find some JSON data in this
format (`fuel.json`):

```json
[ {
  "state" : "IL",
  "city" : "Chicago",
  "fuel_type_code" : "CNG",
  "zip" : "60607",
  "station_name" : "Clean Energy - Yellow Cab",
  "cards_accepted" : "A D M V Voyager Wright_Exp CleanEnergy",
  "street_address" : "540 W Grenshaw"
}, ...
```

And then use aeson and pandoc to parse the JSON and create
the Word document:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Text.Pandoc.Builder
import Text.Pandoc
import Data.Monoid ((<>), mempty, mconcat)
import Data.Aeson
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.List (intersperse)

data Station = Station{
    address        :: String
  , name           :: String
  , cardsAccepted  :: [String]
  } deriving Show

instance FromJSON Station where
    parseJSON (Object v) = Station <$>
       v .: "street_address" <*>
       v .: "station_name" <*>
       (words <$> (v .:? "cards_accepted" .!= ""))
    parseJSON _          = mzero

createLetter :: [Station] -> Pandoc
createLetter stations = doc $
    para "Dear Boss:" <>
    para "Here are the CNG stations that accept Voyager cards:" <>
    simpleTable [plain "Station", plain "Address", plain "Cards accepted"]
           (map stationToRow stations) <>
    para "Your loyal servant," <>
    plain (image "JohnHancock.png" "" mempty)
  where
    stationToRow station =
      [ plain (text $ name station)
      , plain (text $ address station)
      , plain (mconcat $ intersperse linebreak
                       $ map text $ cardsAccepted station)
      ]

main :: IO ()
main = do
  json <- BL.readFile "fuel.json"
  let letter = case decode json of
                    Just stations -> createLetter [s | s <- stations,
                                        "Voyager" `elem` cardsAccepted s]
                    Nothing       -> error "Could not decode JSON"
  docx <- runIO (writeDocx def letter) >>= handleError
  BL.writeFile "letter.docx" docx
  putStrLn "Created letter.docx"
```

Voila!  You've written the letter without using Word and
without looking at the data.

# Templates and other data files

readDataFile

getTemplate

# Handling errors and warnings

Text.Pandoc.Error
Text.Pandoc.Logging
getLog
verbosity

# Walking the AST

Text.Pandoc.Walk for AST transformations
walk and query, with examples
(don't bother mentioning syb)

# Filters

Filters: see filters.md

applyFilters, applyLuaFilters from Text.Pandoc.App.

# Creating a PDF

Text.Pandoc.PDF

# Creating a front-end

Text.Pandoc.App

