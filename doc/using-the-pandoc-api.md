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
*writers*, which render this AST into various output formats.
Pictorially:

```
[input format] ==reader==> [Pandoc AST] ==writer==> [output format]
```

This architecture allows pandoc to perform $M \times N$
conversions with $M$ readers and $N$ writers.

The Pandoc AST is defined in the
[pandoc-types](https://hackage.haskell.org/package/pandoc-types)
package.  You should start by looking at the Haddock
documentation for [Text.Pandoc.Definition].  As you'll see, a
`Pandoc` is composed of some metadata and a list of `Block`s.
There are various kinds of `Block`, including `Para`
(paragraph), `Header` (section heading), and `BlockQuote`.  Some
of the `Block`s (like `BlockQuote`) contain lists of `Block`s,
while others (like `Para`) contain lists of `Inline`s, and still
others (like `CodeBlock`) contain plain text or nothing.
`Inline`s are the basic elements of paragraphs.  The distinction
between `Block` and `Inline` in the type system makes it
impossible to represent, for example, a link (`Inline`) whose
link text is a block quote (`Block`).  This expressive
limitation is mostly a help rather than a hindrance, since many
of the formats pandoc supports have similar limitations.

The best way to explore the pandoc AST is to use `pandoc -t
native`, which will display the AST corresponding to some
Markdown input:

```
% echo -e "1. *foo*\n2. bar" | pandoc -t native
[OrderedList (1,Decimal,Period)
 [[Plain [Emph [Str "foo"]]]
 ,[Plain [Str "bar"]]]]
```

# A simple example

Here is a simple example of the use of a pandoc reader and
writer to perform a conversion:

```haskell
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
readMarkdown :: (PandocMonad m, ToSources a)
             => ReaderOptions
             -> a
             -> m Pandoc
writeRST     :: PandocMonad m
             => WriterOptions
             -> Pandoc
             -> m Text
```

The `PandocMonad m =>` part is a typeclass constraint.
It says that `readMarkdown` and `writeRST` define computations
that can be used in any instance of the `PandocMonad`
type class.  `PandocMonad` is defined in the module
[Text.Pandoc.Class].

Two instances of `PandocMonad` are provided: `PandocIO` and
`PandocPure`. The difference is that computations run in
`PandocIO` are allowed to do IO (for example, read a file),
while computations in `PandocPure` are free of any side effects.
`PandocPure` is useful for sandboxed environments, when you want
to prevent users from doing anything malicious.  To run the
conversion in `PandocIO`, use `runIO` (as above).  To run it in
`PandocPure`, use `runPure`.

As you can see from the Haddocks, [Text.Pandoc.Class]
exports many auxiliary functions that can be used in any
instance of `PandocMonad`.  For example:

```haskell
-- | Get the verbosity level.
getVerbosity :: PandocMonad m => m Verbosity

-- | Set the verbosity level.
setVerbosity :: PandocMonad m => Verbosity -> m ()

-- Get the accumulated log messages (in temporal order).
getLog :: PandocMonad m => m [LogMessage]
getLog = reverse <$> getsCommonState stLog

-- | Log a message using 'logOutput'.  Note that 'logOutput' is
-- called only if the verbosity level exceeds the level of the
-- message, but the message is added to the list of log messages
-- that will be retrieved by 'getLog' regardless of its verbosity level.
report :: PandocMonad m => LogMessage -> m ()

-- | Fetch an image or other item from the local filesystem or the net.
-- Returns raw content and maybe mime type.
fetchItem :: PandocMonad m
          => Text
          -> m (B.ByteString, Maybe MimeType)

-- Set the resource path searched by 'fetchItem'.
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

Note that `PandocIO` is an instance of `MonadIO`, so you can
use `liftIO` to perform arbitrary IO operations inside a pandoc
conversion chain.

`readMarkdown` is polymorphic in its second argument, which
can be any type that is an instance of the `ToSources`
typeclass.  You can use `Text`, as in the example above.
But you can also use `[(FilePath, Text)]`, if the input comes
from multiple files and you want to track source positions
accurately.

# Options

The first argument of each reader or writer is for
options controlling the behavior of the reader or writer:
`ReaderOptions` for readers and `WriterOptions`
for writers.  These are defined in [Text.Pandoc.Options].  It is
a good idea to study these options to see what can be adjusted.

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
    where `template` is a `Template Text` from
    [Text.Pandoc.Templates] containing the template's
    contents (not the path).

2.  `readerExtensions` and `writerExtensions`:  These specify
    the extensions to be used in parsing and rendering.
    Extensions are defined in [Text.Pandoc.Extensions].

# Builder

Sometimes it's useful to construct a Pandoc document
programmatically.  To make this easier we provide the
module [Text.Pandoc.Builder] `pandoc-types`.

Because concatenating lists is slow, we use special
types `Inlines` and `Blocks` that wrap a `Sequence` of
`Inline` and `Block` elements.  These are instances
of the Monoid typeclass and can easily be concatenated:

```haskell
import Text.Pandoc.Builder

mydoc :: Pandoc
mydoc = doc $ header 1 (text (T.pack "Hello!"))
           <> para (emph (text (T.pack "hello world")) <> text (T.pack "."))

main :: IO ()
main = print mydoc
```

If you use the `OverloadedStrings` pragma, you can
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
    address        :: T.Text
  , name           :: T.Text
  , cardsAccepted  :: [T.Text]
  } deriving Show

instance FromJSON Station where
    parseJSON (Object v) = Station <$>
       v .: "street_address" <*>
       v .: "station_name" <*>
       (T.words <$> (v .:? "cards_accepted" .!= ""))
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

# Data files

Pandoc has a number of data files, which can be found in the
`data/` subdirectory of the repository.  These are installed
with pandoc (or, if pandoc was compiled with the
`embed_data_files` flag, they are embedded in the binary).
You can retrieve data files using `readDataFile` from
Text.Pandoc.Class.  `readDataFile` will first look for the
file in the "user data directory" (`setUserDataDir`,
`getUserDataDir`), and if it is not found there, it will
return the default installed with the system.
To force the use of the default, `setUserDataDir Nothing`.

# Templates

Pandoc has its own template system, described in the User's
Guide.  To retrieve the default template for a system,
use `getDefaultTemplate` from [Text.Pandoc.Templates].
Note that this looks first in the
`templates` subdirectory of the user data directory, allowing
users to override the system defaults.  If you want to disable
this behavior, use `setUserDataDir Nothing`.

To render a template, use `renderTemplate'`, which takes two
arguments, a template (Text) and a context (any instance
of ToJSON).  If you want to create a context from the metadata
part of a Pandoc document, use `metaToJSON'` from
[Text.Pandoc.Writers.Shared].  If you also want to incorporate
values from variables, use `metaToJSON` instead, and make sure
`writerVariables` is set in `WriterOptions`.


# Handling errors and warnings

`runIO` and `runPure` return an `Either PandocError a`. All errors
raised in running a `PandocMonad` computation will be trapped
and returned as a `Left` value, so they can be handled by
the calling program.  To see the constructors for `PandocError`,
see the documentation for [Text.Pandoc.Error].

To raise a `PandocError` from inside a `PandocMonad` computation,
use `throwError`.

In addition to errors, which stop execution of the conversion
pipeline, one can generate informational messages.
Use `report` from [Text.Pandoc.Class] to issue a `LogMessage`.
For a list of constructors for `LogMessage`, see
[Text.Pandoc.Logging].  Note that each type of log message
is associated with a verbosity level.  The verbosity level
(`setVerbosity`/`getVerbosity`) determines whether the report
will be printed to stderr (when running in `PandocIO`), but
regardless of verbosity level, all reported messages are stored
internally and may be retrieved using `getLog`.

# Walking the AST

It is often useful to walk the Pandoc AST either to extract
information (e.g., what are all the URLs linked to in this
document?, do all the code samples compile?) or to transform a
document (e.g., increase the level of every section header,
remove emphasis, or replace specially marked code blocks with
images).  To make this easier and more efficient, `pandoc-types`
includes a module [Text.Pandoc.Walk].

Here's the essential documentation:

```haskell
class Walkable a b where
  -- | @walk f x@ walks the structure @x@ (bottom up) and replaces every
  -- occurrence of an @a@ with the result of applying @f@ to it.
  walk  :: (a -> a) -> b -> b
  walk f = runIdentity . walkM (return . f)
  -- | A monadic version of 'walk'.
  walkM :: (Monad m, Functor m) => (a -> m a) -> b -> m b
  -- | @query f x@ walks the structure @x@ (bottom up) and applies @f@
  -- to every @a@, appending the results.
  query :: Monoid c => (a -> c) -> b -> c
```

`Walkable` instances are defined for most combinations of
Pandoc types.  For example, the `Walkable Inline Block`
instance allows you to take a function `Inline -> Inline`
and apply it over every inline in a `Block`.  And
`Walkable [Inline] Pandoc` allows you to take a function
`[Inline] -> [Inline]` and apply it over every maximal
list of `Inline`s in a `Pandoc`.

Here's a simple example of a function that promotes
the levels of headers:

```haskell
promoteHeaderLevels :: Pandoc -> Pandoc
promoteHeaderLevels = walk promote
  where promote :: Block -> Block
        promote (Header lev attr ils) = Header (lev + 1) attr ils
        promote x = x
```

`walkM` is a monadic version of `walk`; it can be used, for
example, when you need your transformations to perform IO
operations, use PandocMonad operations, or update internal
state.  Here's an example using the State monad to add unique
identifiers to each code block:

```haskell
addCodeIdentifiers :: Pandoc -> Pandoc
addCodeIdentifiers doc = evalState (walkM addCodeId doc) 1
  where addCodeId :: Block -> State Int Block
        addCodeId (CodeBlock (_,classes,kvs) code) = do
          curId <- get
          put (curId + 1)
          return $ CodeBlock (show curId,classes,kvs) code
        addCodeId x = return x
```

`query` is used to collect information from the AST.
Its argument is a query function that produces a result
in some monoidal type (e.g. a list).  The results are
concatenated together.  Here's an example that returns a
list of the URLs linked to in a document:

```haskell
listURLs :: Pandoc -> [Text]
listURLs = query urls
  where urls (Link _ _ (src, _)) = [src]
        urls _                   = []
```

# Creating a front-end

All of the functionality of the command-line program `pandoc`
has been abstracted out in `convertWithOpts` in
the module [Text.Pandoc.App].  Creating a GUI front-end for
pandoc is thus just a matter of populating the `Opts`
structure and calling this function.

# Notes on using pandoc in web applications

1. Pandoc's parsers can exhibit pathological behavior on some
   inputs.  So it is always a good idea to wrap uses of pandoc
   in a timeout function (e.g. `System.Timeout.timeout` from `base`)
   to prevent DoS attacks.

2. If pandoc generates HTML from untrusted user input, it is
   always a good idea to filter the generated HTML through
   a sanitizer (such as `xss-sanitize`) to avoid security
   problems.

3. Using `runPure` rather than `runIO` will ensure that
   pandoc's functions perform no IO operations (e.g. writing
   files).  If some resources need to be made available, a
   "fake environment" is provided inside the state available
   to `runPure` (see `PureState` and its associated functions
   in [Text.Pandoc.Class]).  It is also possible to write
   a custom instance of `PandocMonad` that, for example,
   makes wiki resources available as files in the fake environment,
   while isolating pandoc from the rest of the system.


[Text.Pandoc.Definition]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html
[Text.Pandoc.Walk]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Walk.html
[Text.Pandoc.Class]: https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Class.html
[Text.Pandoc.Options]: https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Options.html
[Text.Pandoc.Extensions]: https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Extensions.html
[Text.Pandoc.Builder]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Builder.html
[Text.Pandoc.Templates]: https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Templates.html
[Text.Pandoc.Logging]: https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Logging.html
[Text.Pandoc.App]: https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-App.html
[Text.Pandoc.Error]: https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Error.html
[Text.Pandoc.Writers.Shared]: https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Writers-Shared.html
