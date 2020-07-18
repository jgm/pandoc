{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.DocBook
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of DocBook XML to 'Pandoc' document.
-}
module Text.Pandoc.Readers.DocBook ( readDocBook ) where
import Control.Monad.State.Strict
import Data.Char (isSpace, toUpper)
import Data.Default
import Data.Either (rights)
import Data.Foldable (asum)
import Data.Generics
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Options
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Shared (crFilter, safeRead)
import Text.TeXMath (readMathML, writeTeX)
import Text.XML.Light

{-

List of all DocBook tags, with [x] indicating implemented,
[o] meaning intentionally left unimplemented (pass through):

[o] abbrev - An abbreviation, especially one followed by a period
[x] abstract - A summary
[o] accel - A graphical user interface (GUI) keyboard shortcut
[x] ackno - Acknowledgements in an Article
[o] acronym - An often pronounceable word made from the initial
[o] action - A response to a user event
[o] address - A real-world address, generally a postal address
[ ] affiliation - The institutional affiliation of an individual
[ ] alt - Text representation for a graphical element
[o] anchor - A spot in the document
[x] answer - An answer to a question posed in a QandASet
[x] appendix - An appendix in a Book or Article
[x] appendixinfo - Meta-information for an Appendix
[o] application - The name of a software program
[x] area - A region defined for a Callout in a graphic or code example
[x] areaset - A set of related areas in a graphic or code example
[x] areaspec - A collection of regions in a graphic or code example
[ ] arg - An argument in a CmdSynopsis
[x] article - An article
[x] articleinfo - Meta-information for an Article
[ ] artpagenums - The page numbers of an article as published
[x] attribution - The source of a block quote or epigraph
[ ] audiodata - Pointer to external audio data
[ ] audioobject - A wrapper for audio data and its associated meta-information
[x] author - The name of an individual author
[ ] authorblurb - A short description or note about an author
[x] authorgroup - Wrapper for author information when a document has
    multiple authors or collaborators
[x] authorinitials - The initials or other short identifier for an author
[o] beginpage - The location of a page break in a print version of the document
[ ] bibliocoverage - The spatial or temporal coverage of a document
[x] bibliodiv - A section of a Bibliography
[x] biblioentry - An entry in a Bibliography
[x] bibliography - A bibliography
[ ] bibliographyinfo - Meta-information for a Bibliography
[ ] biblioid - An identifier for a document
[o] bibliolist - A wrapper for a set of bibliography entries
[ ] bibliomisc - Untyped bibliographic information
[x] bibliomixed - An entry in a Bibliography
[ ] bibliomset - A cooked container for related bibliographic information
[ ] biblioref - A cross reference to a bibliographic entry
[ ] bibliorelation - The relationship of a document to another
[ ] biblioset - A raw container for related bibliographic information
[ ] bibliosource - The source of a document
[ ] blockinfo - Meta-information for a block element
[x] blockquote - A quotation set off from the main text
[x] book - A book
[x] bookinfo - Meta-information for a Book
[x] bridgehead - A free-floating heading
[x] callout - A “called out” description of a marked Area
[x] calloutlist - A list of Callouts
[x] caption - A caption
[x] caution - A note of caution
[x] chapter - A chapter, as of a book
[x] chapterinfo - Meta-information for a Chapter
[ ] citation - An inline bibliographic reference to another published work
[ ] citebiblioid - A citation of a bibliographic identifier
[ ] citerefentry - A citation to a reference page
[ ] citetitle - The title of a cited work
[ ] city - The name of a city in an address
[x] classname - The name of a class, in the object-oriented programming sense
[ ] classsynopsis - The syntax summary for a class definition
[ ] classsynopsisinfo - Information supplementing the contents of
    a ClassSynopsis
[ ] cmdsynopsis - A syntax summary for a software command
[ ] co - The location of a callout embedded in text
[x] code - An inline code fragment
[x] col - Specifications for a column in an HTML table
[x] colgroup - A group of columns in an HTML table
[ ] collab - Identifies a collaborator
[ ] collabname - The name of a collaborator
[ ] colophon - Text at the back of a book describing facts about its production
[x] colspec - Specifications for a column in a table
[x] command - The name of an executable program or other software command
[x] computeroutput - Data, generally text, displayed or presented by a computer
[ ] confdates - The dates of a conference for which a document was written
[ ] confgroup - A wrapper for document meta-information about a conference
[ ] confnum - An identifier, frequently numerical, associated with a conference for which a document was written
[ ] confsponsor - The sponsor of a conference for which a document was written
[ ] conftitle - The title of a conference for which a document was written
[x] constant - A programming or system constant
[ ] constraint - A constraint in an EBNF production
[ ] constraintdef - The definition of a constraint in an EBNF production
[ ] constructorsynopsis - A syntax summary for a constructor
[ ] contractnum - The contract number of a document
[ ] contractsponsor - The sponsor of a contract
[ ] contrib - A summary of the contributions made to a document by a
    credited source
[ ] copyright - Copyright information about a document
[ ] coref - A cross reference to a co
[ ] corpauthor - A corporate author, as opposed to an individual
[ ] corpcredit - A corporation or organization credited in a document
[ ] corpname - The name of a corporation
[ ] country - The name of a country
[ ] database - The name of a database, or part of a database
[x] date - The date of publication or revision of a document
[ ] dedication - A wrapper for the dedication section of a book
[ ] destructorsynopsis - A syntax summary for a destructor
[ ] edition - The name or number of an edition of a document
[ ] editor - The name of the editor of a document
[x] email - An email address
[x] emphasis - Emphasized text
[x] entry - A cell in a table
[ ] entrytbl - A subtable appearing in place of an Entry in a table
[x] envar - A software environment variable
[x] epigraph - A short inscription at the beginning of a document or component
    note:  also handle embedded attribution tag
[x] equation - A displayed mathematical equation
[ ] errorcode - An error code
[ ] errorname - An error name
[ ] errortext - An error message.
[ ] errortype - The classification of an error message
[ ] example - A formal example, with a title
[ ] exceptionname - The name of an exception
[ ] fax - A fax number
[ ] fieldsynopsis - The name of a field in a class definition
[x] figure - A formal figure, generally an illustration, with a title
[x] filename - The name of a file
[ ] firstname - The first name of a person
[ ] firstterm - The first occurrence of a term
[x] footnote - A footnote
[ ] footnoteref - A cross reference to a footnote (a footnote mark)
[x] foreignphrase - A word or phrase in a language other than the primary
    language of the document
[x] formalpara - A paragraph with a title
[ ] funcdef - A function (subroutine) name and its return type
[ ] funcparams - Parameters for a function referenced through a function
    pointer in a synopsis
[ ] funcprototype - The prototype of a function
[ ] funcsynopsis - The syntax summary for a function definition
[ ] funcsynopsisinfo - Information supplementing the FuncDefs of a FuncSynopsis
[x] function - The name of a function or subroutine, as in a
    programming language
[x] glossary - A glossary
[x] glossaryinfo - Meta-information for a Glossary
[x] glossdef - A definition in a GlossEntry
[x] glossdiv - A division in a Glossary
[x] glossentry - An entry in a Glossary or GlossList
[x] glosslist - A wrapper for a set of GlossEntrys
[x] glosssee - A cross-reference from one GlossEntry to another
[x] glossseealso - A cross-reference from one GlossEntry to another
[x] glossterm - A glossary term
[ ] graphic - A displayed graphical object (not an inline)
    Note: in DocBook v5 `graphic` is discarded
[ ] graphicco - A graphic that contains callout areas
    Note: in DocBook v5 `graphicco` is discarded
[ ] group - A group of elements in a CmdSynopsis
[ ] guibutton - The text on a button in a GUI
[ ] guiicon - Graphic and/or text appearing as a icon in a GUI
[ ] guilabel - The text of a label in a GUI
[x] guimenu - The name of a menu in a GUI
[x] guimenuitem - The name of a terminal menu item in a GUI
[x] guisubmenu - The name of a submenu in a GUI
[ ] hardware - A physical part of a computer system
[ ] highlights - A summary of the main points of the discussed component
[ ] holder - The name of the individual or organization that holds a copyright
[o] honorific - The title of a person
[ ] html:form - An HTML form
[x] imagedata - Pointer to external image data (only `fileref` attribute
    implemented but not `entityref` which would require parsing of the DTD)
[x] imageobject - A wrapper for image data and its associated meta-information
[ ] imageobjectco - A wrapper for an image object with callouts
[x] important - An admonition set off from the text
[x] index - An index
[x] indexdiv - A division in an index
[x] indexentry - An entry in an index
[x] indexinfo - Meta-information for an Index
[x] indexterm - A wrapper for terms to be indexed
[x] info - A wrapper for information about a component or other block. (DocBook v5)
[x] informalequation - A displayed mathematical equation without a title
[x] informalexample - A displayed example without a title
[ ] informalfigure - A untitled figure
[ ] informaltable - A table without a title
[ ] initializer - The initializer for a FieldSynopsis
[x] inlineequation - A mathematical equation or expression occurring inline
[ ] inlinegraphic - An object containing or pointing to graphical data
    that will be rendered inline
[x] inlinemediaobject - An inline media object (video, audio, image, and so on)
[ ] interface - An element of a GUI
[ ] interfacename - The name of an interface
[ ] invpartnumber - An inventory part number
[ ] isbn - The International Standard Book Number of a document
[ ] issn - The International Standard Serial Number of a periodical
[ ] issuenum - The number of an issue of a journal
[x] itemizedlist - A list in which each entry is marked with a bullet or
    other dingbat
[ ] itermset - A set of index terms in the meta-information of a document
[ ] jobtitle - The title of an individual in an organization
[x] keycap - The text printed on a key on a keyboard
[ ] keycode - The internal, frequently numeric, identifier for a key
    on a keyboard
[x] keycombo - A combination of input actions
[ ] keysym - The symbolic name of a key on a keyboard
[ ] keyword - One of a set of keywords describing the content of a document
[ ] keywordset - A set of keywords describing the content of a document
[ ] label - A label on a Question or Answer
[ ] legalnotice - A statement of legal obligations or requirements
[ ] lhs - The left-hand side of an EBNF production
[ ] lineage - The portion of a person's name indicating a relationship to
    ancestors
[ ] lineannotation - A comment on a line in a verbatim listing
[x] link - A hypertext link
[x] listitem - A wrapper for the elements of a list item
[x] literal - Inline text that is some literal value
[x] literallayout - A block of text in which line breaks and white space are
    to be reproduced faithfully
[ ] lot - A list of the titles of formal objects (as tables or figures) in
    a document
[ ] lotentry - An entry in a list of titles
[ ] manvolnum - A reference volume number
[x] markup - A string of formatting markup in text that is to be
    represented literally
[x] mathphrase - A mathematical phrase, an expression that can be represented
    with ordinary text and a small amount of markup
[ ] medialabel - A name that identifies the physical medium on which some
    information resides
[x] mediaobject - A displayed media object (video, audio, image, etc.)
[ ] mediaobjectco - A media object that contains callouts
[x] member - An element of a simple list
[x] menuchoice - A selection or series of selections from a menu
[ ] methodname - The name of a method
[ ] methodparam - Parameters to a method
[ ] methodsynopsis - A syntax summary for a method
[x] mml:math - A MathML equation
[ ] modespec - Application-specific information necessary for the
    completion of an OLink
[ ] modifier - Modifiers in a synopsis
[ ] mousebutton - The conventional name of a mouse button
[ ] msg - A message in a message set
[ ] msgaud - The audience to which a message in a message set is relevant
[ ] msgentry - A wrapper for an entry in a message set
[ ] msgexplan - Explanatory material relating to a message in a message set
[ ] msginfo - Information about a message in a message set
[ ] msglevel - The level of importance or severity of a message in a message set
[ ] msgmain - The primary component of a message in a message set
[ ] msgorig - The origin of a message in a message set
[ ] msgrel - A related component of a message in a message set
[ ] msgset - A detailed set of messages, usually error messages
[ ] msgsub - A subcomponent of a message in a message set
[ ] msgtext - The actual text of a message component in a message set
[ ] nonterminal - A non-terminal in an EBNF production
[x] note - A message set off from the text
[ ] objectinfo - Meta-information for an object
[ ] olink - A link that addresses its target indirectly, through an entity
[ ] ooclass - A class in an object-oriented programming language
[ ] ooexception - An exception in an object-oriented programming language
[ ] oointerface - An interface in an object-oriented programming language
[x] option - An option for a software command
[x] optional - Optional information
[x] orderedlist - A list in which each entry is marked with a sequentially
    incremented label
[ ] orgdiv - A division of an organization
[ ] orgname - The name of an organization other than a corporation
[ ] otheraddr - Uncategorized information in address
[ ] othercredit - A person or entity, other than an author or editor,
    credited in a document
[ ] othername - A component of a persons name that is not a first name,
    surname, or lineage
[ ] package - A package
[ ] pagenums - The numbers of the pages in a book, for use in a bibliographic
    entry
[x] para - A paragraph
[ ] paramdef - Information about a function parameter in a programming language
[x] parameter - A value or a symbolic reference to a value
[ ] part - A division in a book
[ ] partinfo - Meta-information for a Part
[ ] partintro - An introduction to the contents of a part
[ ] personblurb - A short description or note about a person
[ ] personname - The personal name of an individual
[ ] phone - A telephone number
[x] phrase - A span of text
[ ] pob - A post office box in an address
[ ] postcode - A postal code in an address
[x] preface - Introductory matter preceding the first chapter of a book
[ ] prefaceinfo - Meta-information for a Preface
[ ] primary - The primary word or phrase under which an index term should be
    sorted
[ ] primaryie - A primary term in an index entry, not in the text
[ ] printhistory - The printing history of a document
[x] procedure - A list of operations to be performed in a well-defined sequence
[ ] production - A production in a set of EBNF productions
[ ] productionrecap - A cross-reference to an EBNF production
[ ] productionset - A set of EBNF productions
[ ] productname - The formal name of a product
[ ] productnumber - A number assigned to a product
[x] programlisting - A literal listing of all or part of a program
[ ] programlistingco - A program listing with associated areas used in callouts
[x] prompt - A character or string indicating the start of an input field in
    a computer display
[ ] property - A unit of data associated with some part of a computer system
[ ] pubdate - The date of publication of a document
[ ] publisher - The publisher of a document
[ ] publishername - The name of the publisher of a document
[ ] pubsnumber - A number assigned to a publication other than an ISBN or ISSN
    or inventory part number
[x] qandadiv - A titled division in a QandASet
[o] qandaentry - A question/answer set within a QandASet
[o] qandaset - A question-and-answer set
[x] question - A question in a QandASet
[x] quote - An inline quotation
[ ] refclass - The scope or other indication of applicability of a
    reference entry
[ ] refdescriptor - A description of the topic of a reference page
[ ] refentry - A reference page (originally a UNIX man-style reference page)
[ ] refentryinfo - Meta-information for a Refentry
[ ] refentrytitle - The title of a reference page
[ ] reference - A collection of reference entries
[ ] referenceinfo - Meta-information for a Reference
[ ] refmeta - Meta-information for a reference entry
[ ] refmiscinfo - Meta-information for a reference entry other than the title
    and volume number
[ ] refname - The name of (one of) the subject(s) of a reference page
[ ] refnamediv - The name, purpose, and classification of a reference page
[ ] refpurpose - A short (one sentence) synopsis of the topic of a reference
    page
[x] refsect1 - A major subsection of a reference entry
[x] refsect1info - Meta-information for a RefSect1
[x] refsect2 - A subsection of a RefSect1
[x] refsect2info - Meta-information for a RefSect2
[x] refsect3 - A subsection of a RefSect2
[x] refsect3info - Meta-information for a RefSect3
[x] refsection - A recursive section in a refentry
[x] refsectioninfo - Meta-information for a refsection
[ ] refsynopsisdiv - A syntactic synopsis of the subject of the reference page
[ ] refsynopsisdivinfo - Meta-information for a RefSynopsisDiv
[ ] releaseinfo - Information about a particular release of a document
[ ] remark - A remark (or comment) intended for presentation in a draft
    manuscript
[x] replaceable - Content that may or must be replaced by the user
[ ] returnvalue - The value returned by a function
[ ] revdescription - A extended description of a revision to a document
[ ] revhistory - A history of the revisions to a document
[ ] revision - An entry describing a single revision in the history of the
    revisions to a document
[ ] revnumber - A document revision number
[ ] revremark - A description of a revision to a document
[ ] rhs - The right-hand side of an EBNF production
[x] row - A row in a table
[ ] sbr - An explicit line break in a command synopsis
[x] screen - Text that a user sees or might see on a computer screen
[o] screenco - A screen with associated areas used in callouts
[o] screeninfo - Information about how a screen shot was produced
[ ] screenshot - A representation of what the user sees or might see on a
    computer screen
[ ] secondary - A secondary word or phrase in an index term
[ ] secondaryie - A secondary term in an index entry, rather than in the text
[x] sect1 - A top-level section of document
[x] sect1info - Meta-information for a Sect1
[x] sect2 - A subsection within a Sect1
[x] sect2info - Meta-information for a Sect2
[x] sect3 - A subsection within a Sect2
[x] sect3info - Meta-information for a Sect3
[x] sect4 - A subsection within a Sect3
[x] sect4info - Meta-information for a Sect4
[x] sect5 - A subsection within a Sect4
[x] sect5info - Meta-information for a Sect5
[x] section - A recursive section
[x] sectioninfo - Meta-information for a recursive section
[x] see - Part of an index term directing the reader instead to another entry
    in the index
[x] seealso - Part of an index term directing the reader also to another entry
    in the index
[ ] seealsoie - A See also entry in an index, rather than in the text
[ ] seeie - A See entry in an index, rather than in the text
[x] seg - An element of a list item in a segmented list
[x] seglistitem - A list item in a segmented list
[x] segmentedlist - A segmented list, a list of sets of elements
[x] segtitle - The title of an element of a list item in a segmented list
[ ] seriesvolnums - Numbers of the volumes in a series of books
[ ] set - A collection of books
[ ] setindex - An index to a set of books
[ ] setindexinfo - Meta-information for a SetIndex
[ ] setinfo - Meta-information for a Set
[ ] sgmltag - A component of SGML markup
[ ] shortaffil - A brief description of an affiliation
[ ] shortcut - A key combination for an action that is also accessible through
    a menu
[ ] sidebar - A portion of a document that is isolated from the main
    narrative flow
[ ] sidebarinfo - Meta-information for a Sidebar
[x] simpara - A paragraph that contains only text and inline markup, no block
    elements
[x] simplelist - An undecorated list of single words or short phrases
[ ] simplemsgentry - A wrapper for a simpler entry in a message set
[x] simplesect - A section of a document with no subdivisions
[ ] spanspec - Formatting information for a spanned column in a table
[ ] state - A state or province in an address
[x] step - A unit of action in a procedure
[ ] stepalternatives - Alternative steps in a procedure
[ ] street - A street address in an address
[ ] structfield - A field in a structure (in the programming language sense)
[ ] structname - The name of a structure (in the programming language sense)
[ ] subject - One of a group of terms describing the subject matter of a
    document
[ ] subjectset - A set of terms describing the subject matter of a document
[ ] subjectterm - A term in a group of terms describing the subject matter of
    a document
[x] subscript - A subscript (as in H2O, the molecular formula for water)
[x] substeps - A wrapper for steps that occur within steps in a procedure
[x] subtitle - The subtitle of a document
[x] superscript - A superscript (as in x2, the mathematical notation for x
    multiplied by itself)
[ ] surname - A family name; in western cultures the last name
[ ] svg:svg - An SVG graphic
[x] symbol - A name that is replaced by a value before processing
[ ] synopfragment - A portion of a CmdSynopsis broken out from the main body
    of the synopsis
[ ] synopfragmentref - A reference to a fragment of a command synopsis
[ ] synopsis - A general-purpose element for representing the syntax of
    commands or functions
[x] systemitem - A system-related item or term
[ ] table - A formal table in a document
[ ] task - A task to be completed
[ ] taskprerequisites - The prerequisites for a task
[ ] taskrelated - Information related to a task
[ ] tasksummary - A summary of a task
[x] tbody - A wrapper for the rows of a table or informal table
[x] td - A table entry in an HTML table
[x] term - The word or phrase being defined or described in a variable list
[ ] termdef - An inline term definition
[ ] tertiary - A tertiary word or phrase in an index term
[ ] tertiaryie - A tertiary term in an index entry, rather than in the text
[ ] textdata - Pointer to external text data
[ ] textobject - A wrapper for a text description of an object and its
    associated meta-information
[ ] tfoot - A table footer consisting of one or more rows
[x] tgroup - A wrapper for the main content of a table, or part of a table
[x] th - A table header entry in an HTML table
[x] thead - A table header consisting of one or more rows
[x] tip - A suggestion to the user, set off from the text
[x] title - The text of the title of a section of a document or of a formal
    block-level element
[x] titleabbrev - The abbreviation of a Title
[x] toc - A table of contents
[x] tocback - An entry in a table of contents for a back matter component
[x] tocchap - An entry in a table of contents for a component in the body of
    a document
[x] tocentry - A component title in a table of contents
[x] tocfront - An entry in a table of contents for a front matter component
[x] toclevel1 - A top-level entry within a table of contents entry for a
    chapter-like component
[x] toclevel2 - A second-level entry within a table of contents entry for a
    chapter-like component
[x] toclevel3 - A third-level entry within a table of contents entry for a
    chapter-like component
[x] toclevel4 - A fourth-level entry within a table of contents entry for a
    chapter-like component
[x] toclevel5 - A fifth-level entry within a table of contents entry for a
    chapter-like component
[x] tocpart - An entry in a table of contents for a part of a book
[ ] token - A unit of information
[x] tr - A row in an HTML table
[ ] trademark - A trademark
[x] type - The classification of a value
[x] ulink - A link that addresses its target by means of a URL
    (Uniform Resource Locator)
[x] uri - A Uniform Resource Identifier
[x] userinput - Data entered by the user
[x] varargs - An empty element in a function synopsis indicating a variable
    number of arguments
[x] variablelist - A list in which each entry is composed of a set of one or
    more terms and an associated description
[x] varlistentry - A wrapper for a set of terms and the associated description
    in a variable list
[x] varname - The name of a variable
[ ] videodata - Pointer to external video data
[ ] videoobject - A wrapper for video data and its associated meta-information
[ ] void - An empty element in a function synopsis indicating that the
    function in question takes no arguments
[ ] volumenum - The volume number of a document in a set (as of books in a set
    or articles in a journal)
[x] warning - An admonition set off from the text
[x] wordasword - A word meant specifically as a word and not representing
    anything else
[x] xref - A cross reference to another part of the document
[ ] year - The year of publication of a document
[x] ?asciidoc-br? - line break from asciidoc docbook output
-}

type DB m = StateT DBState m

data DBState = DBState{ dbSectionLevel :: Int
                      , dbQuoteType    :: QuoteType
                      , dbMeta         :: Meta
                      , dbBook         :: Bool
                      , dbFigureTitle  :: Inlines
                      , dbFigureId     :: Text
                      , dbContent      :: [Content]
                      } deriving Show

instance Default DBState where
  def = DBState{ dbSectionLevel = 0
               , dbQuoteType = DoubleQuote
               , dbMeta = mempty
               , dbBook = False
               , dbFigureTitle = mempty
               , dbFigureId = mempty
               , dbContent = [] }


readDocBook :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readDocBook _ inp = do
  let tree = normalizeTree . parseXML . handleInstructions $ crFilter inp
  (bs, st') <- flip runStateT (def{ dbContent = tree }) $ mapM parseBlock tree
  return $ Pandoc (dbMeta st') (toList . mconcat $ bs)

-- We treat <?asciidoc-br?> specially (issue #1236), converting it
-- to <br/>, since xml-light doesn't parse the instruction correctly.
-- Other xml instructions are simply removed from the input stream.
handleInstructions :: Text -> Text
handleInstructions = T.pack . handleInstructions' . T.unpack

handleInstructions' :: String -> String
handleInstructions' ('<':'?':'a':'s':'c':'i':'i':'d':'o':'c':'-':'b':'r':'?':'>':xs) = '<':'b':'r':'/':'>': handleInstructions' xs
handleInstructions' xs = case break (=='<') xs of
                             (ys, [])     -> ys
                             ([], '<':zs) -> '<' : handleInstructions' zs
                             (ys, zs)     -> ys ++ handleInstructions' zs

getFigure :: PandocMonad m => Element -> DB m Blocks
getFigure e = do
  tit <- case filterChild (named "title") e of
              Just t  -> getInlines t
              Nothing -> return mempty
  let ident = attrValue "id" e
  modify $ \st -> st{ dbFigureTitle = tit, dbFigureId = ident }
  res <- getBlocks e
  modify $ \st -> st{ dbFigureTitle = mempty, dbFigureId = mempty }
  return res

-- normalize input, consolidating adjacent Text and CRef elements
normalizeTree :: [Content] -> [Content]
normalizeTree = everywhere (mkT go)
  where go :: [Content] -> [Content]
        go (Text (CData CDataRaw _ _):xs) = xs
        go (Text (CData CDataText s1 z):Text (CData CDataText s2 _):xs) =
           Text (CData CDataText (s1 ++ s2) z):xs
        go (Text (CData CDataText s1 z):CRef r:xs) =
           Text (CData CDataText (s1 ++ convertEntity r) z):xs
        go (CRef r:Text (CData CDataText s1 z):xs) =
             Text (CData CDataText (convertEntity r ++ s1) z):xs
        go (CRef r1:CRef r2:xs) =
             Text (CData CDataText (convertEntity r1 ++ convertEntity r2) Nothing):xs
        go xs = xs

convertEntity :: String -> String
convertEntity e = Data.Maybe.fromMaybe (map toUpper e) (lookupEntity e)

-- convenience function to get an attribute value, defaulting to ""
attrValue :: String -> Element -> Text
attrValue attr elt =
  maybe "" T.pack (lookupAttrBy (\x -> qName x == attr) (elAttribs elt))

-- convenience function
named :: Text -> Element -> Bool
named s e = qName (elName e) == T.unpack s

--

addMetadataFromElement :: PandocMonad m => Element -> DB m Blocks
addMetadataFromElement e = do
    case filterChild (named "title") e of
         Nothing -> return ()
         Just z  -> do
           getInlines z >>= addMeta "title"
           addMetaField "subtitle" z
    case filterChild (named "authorgroup") e of
         Nothing -> return ()
         Just z  -> addMetaField "author" z
    addMetaField "subtitle" e
    addMetaField "author" e
    addMetaField "date" e
    addMetaField "release" e
    addMetaField "releaseinfo" e
    return mempty
  where addMetaField fieldname elt =
            case filterChildren (named fieldname) elt of
                   []  -> return ()
                   [z] -> getInlines z >>= addMeta fieldname
                   zs  -> mapM getInlines zs >>= addMeta fieldname

addMeta :: PandocMonad m => ToMetaValue a => Text -> a -> DB m ()
addMeta field val = modify (setMeta field val)

instance HasMeta DBState where
  setMeta field v s =  s {dbMeta = setMeta field v (dbMeta s)}
  deleteMeta field s = s {dbMeta = deleteMeta field (dbMeta s)}

isBlockElement :: Content -> Bool
isBlockElement (Elem e) = qName (elName e) `elem` blockTags
isBlockElement _ = False

blockTags :: [String]
blockTags = ["toc","index","para","formalpara","simpara",
           "ackno","epigraph","blockquote","bibliography","bibliodiv",
           "biblioentry","glossee","glosseealso","glossary",
           "glossdiv","glosslist","chapter","appendix","preface","bridgehead",
           "sect1","sect2","sect3","sect4","sect5","section","simplesect",
           "refsect1","refsect2","refsect3","refsection", "qandadiv",
           "question","answer","abstract","itemizedlist","orderedlist",
           "variablelist","article","book","table","informaltable",
           "informalexample", "linegroup","procedure","substeps",
           "screen","programlisting","example","calloutlist"] ++ admonitionTags

admonitionTags :: [String]
admonitionTags = ["important","caution","note","tip","warning"]

-- Trim leading and trailing newline characters
trimNl :: Text -> Text
trimNl = T.dropAround (== '\n')

-- meld text into beginning of first paragraph of Blocks.
-- assumes Blocks start with a Para; if not, does nothing.
addToStart :: Inlines -> Blocks -> Blocks
addToStart toadd bs =
  case toList bs of
    (Para xs : rest) -> para (toadd <> fromList xs) <> fromList rest
    _                -> bs

-- function that is used by both mediaobject (in parseBlock)
-- and inlinemediaobject (in parseInline)
-- A DocBook mediaobject is a wrapper around a set of alternative presentations
getMediaobject :: PandocMonad m => Element -> DB m Inlines
getMediaobject e = do
  figTitle <- gets dbFigureTitle
  ident <- gets dbFigureId
  (imageUrl, attr) <-
    case filterChild (named "imageobject") e of
      Nothing  -> return (mempty, nullAttr)
      Just z   -> case filterChild (named "imagedata") z of
                    Nothing -> return (mempty, nullAttr)
                    Just i  -> let atVal a = attrValue a i
                                   w = case atVal "width" of
                                         "" -> []
                                         d  -> [("width", d)]
                                   h = case atVal "depth" of
                                         "" -> []
                                         d  -> [("height", d)]
                                   id' = case atVal "id" of
                                           x | T.null x  -> ident
                                             | otherwise -> x
                                   cs = T.words $ atVal "role"
                                   atr = (id', cs, w ++ h)
                               in  return (atVal "fileref", atr)
  let getCaption el = case filterChild (\x -> named "caption" x
                                            || named "textobject" x
                                            || named "alt" x) el of
                        Nothing -> return mempty
                        Just z  -> mconcat <$>
                                         mapM parseInline (elContent z)
  let (capt, title) = if isNull figTitle
                         then (getCaption e, "")
                         else (return figTitle, "fig:")
  fmap (imageWith attr imageUrl title) capt

getBlocks :: PandocMonad m => Element -> DB m Blocks
getBlocks e =  mconcat <$>
                 mapM parseBlock (elContent e)


parseBlock :: PandocMonad m => Content -> DB m Blocks
parseBlock (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock (Text (CData _ s _)) = if all isSpace s
                                     then return mempty
                                     else return $ plain $ trimInlines $ text $ T.pack s
parseBlock (CRef x) = return $ plain $ str $ T.toUpper $ T.pack x
parseBlock (Elem e) =
  case qName (elName e) of
        "toc"   -> skip -- skip TOC, since in pandoc it's autogenerated
        "index" -> skip -- skip index, since page numbers meaningless
        "para"  -> parseMixed para (elContent e)
        "formalpara" -> do
           tit <- case filterChild (named "title") e of
                        Just t  -> (para . strong . (<> str ".")) <$>
                                     getInlines t
                        Nothing -> return mempty
           (tit <>) <$> parseMixed para (elContent e)
        "simpara"  -> parseMixed para (elContent e)
        "ackno"  -> parseMixed para (elContent e)
        "epigraph" -> parseBlockquote
        "blockquote" -> parseBlockquote
        "attribution" -> skip
        "titleabbrev" -> skip
        "authorinitials" -> skip
        "bibliography" -> sect 0
        "bibliodiv" -> sect 1
        "biblioentry" -> parseMixed para (elContent e)
        "bibliomixed" -> parseMixed para (elContent e)
        "equation"         -> para <$> equation e displayMath
        "informalequation" -> para <$> equation e displayMath
        "glosssee" -> para . (\ils -> text "See " <> ils <> str ".")
                         <$> getInlines e
        "glossseealso" -> para . (\ils -> text "See also " <> ils <> str ".")
                         <$> getInlines e
        "glossary" -> sect 0
        "glossdiv" -> definitionList <$>
                  mapM parseGlossEntry (filterChildren (named "glossentry") e)
        "glosslist" -> definitionList <$>
                  mapM parseGlossEntry (filterChildren (named "glossentry") e)
        "chapter" -> modify (\st -> st{ dbBook = True}) >> sect 0
        "appendix" -> sect 0
        "preface" -> sect 0
        "bridgehead" -> para . strong <$> getInlines e
        "sect1" -> sect 1
        "sect2" -> sect 2
        "sect3" -> sect 3
        "sect4" -> sect 4
        "sect5" -> sect 5
        "section" -> gets dbSectionLevel >>= sect . (+1)
        "simplesect" ->
          gets dbSectionLevel >>=
          sectWith (attrValue "id" e,["unnumbered"],[]) . (+1)
        "refsect1" -> sect 1
        "refsect2" -> sect 2
        "refsect3" -> sect 3
        "refsection" -> gets dbSectionLevel >>= sect . (+1)
        l | l `elem` admonitionTags -> parseAdmonition $ T.pack l
        "area" -> skip
        "areaset" -> skip
        "areaspec" -> skip
        "qandadiv" -> gets dbSectionLevel >>= sect . (+1)
        "question" -> addToStart (strong (str "Q:") <> str " ") <$> getBlocks e
        "answer" -> addToStart (strong (str "A:") <> str " ") <$> getBlocks e
        "abstract" -> blockQuote <$> getBlocks e
        "calloutlist" -> bulletList <$> callouts
        "itemizedlist" -> bulletList <$> listitems
        "orderedlist" -> do
          let listStyle = case attrValue "numeration" e of
                               "arabic"     -> Decimal
                               "loweralpha" -> LowerAlpha
                               "upperalpha" -> UpperAlpha
                               "lowerroman" -> LowerRoman
                               "upperroman" -> UpperRoman
                               _            -> Decimal
          let start = fromMaybe 1 $
                      filterElement (named "listitem") e
                       >>= safeRead . attrValue "override"
          orderedListWith (start,listStyle,DefaultDelim)
            <$> listitems
        "variablelist" -> definitionList <$> deflistitems
        "procedure" -> bulletList <$> steps
        "figure" -> getFigure e
        "mediaobject" -> para <$> getMediaobject e
        "caption" -> skip
        "info" -> addMetadataFromElement e
        "articleinfo" -> addMetadataFromElement e
        "sectioninfo" -> skip -- keywords & other metadata
        "refsectioninfo" -> skip -- keywords & other metadata
        "refsect1info" -> skip -- keywords & other metadata
        "refsect2info" -> skip -- keywords & other metadata
        "refsect3info" -> skip -- keywords & other metadata
        "sect1info" -> skip  -- keywords & other metadata
        "sect2info" -> skip  -- keywords & other metadata
        "sect3info" -> skip  -- keywords & other metadata
        "sect4info" -> skip  -- keywords & other metadata
        "sect5info" -> skip  -- keywords & other metadata
        "chapterinfo" -> skip -- keywords & other metadata
        "glossaryinfo" -> skip  -- keywords & other metadata
        "appendixinfo" -> skip  -- keywords & other metadata
        "bookinfo" -> addMetadataFromElement e
        "article" -> modify (\st -> st{ dbBook = False }) >>
                           addMetadataFromElement e >> getBlocks e
        "book" -> modify (\st -> st{ dbBook = True }) >>
                    addMetadataFromElement e >> getBlocks e
        "table" -> parseTable
        "informaltable" -> parseTable
        "informalexample" -> divWith ("", ["informalexample"], []) <$>
                             getBlocks e
        "linegroup" -> lineBlock <$> lineItems
        "literallayout" -> codeBlockWithLang
        "screen" -> codeBlockWithLang
        "programlisting" -> codeBlockWithLang
        "?xml"  -> return mempty
        "title" -> return mempty     -- handled in parent element
        "subtitle" -> return mempty  -- handled in parent element
        _       -> skip >> getBlocks e
   where skip = do
           lift $ report $ IgnoredElement $ T.pack $ qName (elName e)
           return mempty

         parseMixed container conts = do
           let (ils,rest) = break isBlockElement conts
           ils' <- (trimInlines . mconcat) <$> mapM parseInline ils
           let p = if ils' == mempty then mempty else container ils'
           case rest of
                 []     -> return p
                 (r:rs) -> do
                    b <- parseBlock r
                    x <- parseMixed container rs
                    return $ p <> b <> x
         codeBlockWithLang = do
           let classes' = case attrValue "language" e of
                                "" -> []
                                x  -> [x]
           return $ codeBlockWith (attrValue "id" e, classes', [])
                  $ trimNl $ T.pack $ strContentRecursive e
         parseBlockquote = do
            attrib <- case filterChild (named "attribution") e of
                             Nothing  -> return mempty
                             Just z   -> (para . (str "— " <>) . mconcat)
                                         <$>
                                              mapM parseInline (elContent z)
            contents <- getBlocks e
            return $ blockQuote (contents <> attrib)
         listitems = mapM getBlocks $ filterChildren (named "listitem") e
         callouts = mapM getBlocks $ filterChildren (named "callout") e
         deflistitems = mapM parseVarListEntry $ filterChildren
                     (named "varlistentry") e
         steps = mapM getBlocks $ filterChildren (named "step") e
         parseVarListEntry e' = do
                     let terms = filterChildren (named "term") e'
                     let items = filterChildren (named "listitem") e'
                     terms' <- mapM getInlines terms
                     items' <- mapM getBlocks items
                     return (mconcat $ intersperse (str "; ") terms', items')
         parseGlossEntry e' = do
                     let terms = filterChildren (named "glossterm") e'
                     let items = filterChildren (named "glossdef") e'
                     terms' <- mapM getInlines terms
                     items' <- mapM getBlocks items
                     return (mconcat $ intersperse (str "; ") terms', items')
         parseTable = do
                      let isCaption x = named "title" x || named "caption" x
                      capt <- case filterChild isCaption e of
                                    Just t  -> getInlines t
                                    Nothing -> return mempty
                      let e' = fromMaybe e $ filterChild (named "tgroup") e
                      let isColspec x = named "colspec" x || named "col" x
                      let colspecs = case filterChild (named "colgroup") e' of
                                           Just c -> filterChildren isColspec c
                                           _      -> filterChildren isColspec e'
                      let isRow x = named "row" x || named "tr" x
                      headrows <- case filterChild (named "thead") e' of
                                       Just h  -> case filterChild isRow h of
                                                       Just x  -> parseRow x
                                                       Nothing -> return []
                                       Nothing -> return []
                      bodyrows <- case filterChild (named "tbody") e' of
                                       Just b  -> mapM parseRow
                                                  $ filterChildren isRow b
                                       Nothing -> mapM parseRow
                                                  $ filterChildren isRow e'
                      let toAlignment c = case findAttr (unqual "align") c of
                                                Just "left"   -> AlignLeft
                                                Just "right"  -> AlignRight
                                                Just "center" -> AlignCenter
                                                _             -> AlignDefault
                      let toWidth c = do
                            w <- findAttr (unqual "colwidth") c
                            n <- safeRead $ "0" <> T.filter (\x ->
                                                     (x >= '0' && x <= '9')
                                                      || x == '.') (T.pack w)
                            if n > 0 then Just n else Nothing
                      let numrows = case bodyrows of
                                         [] -> 0
                                         xs -> maximum $ map length xs
                      let aligns = case colspecs of
                                     [] -> replicate numrows AlignDefault
                                     cs -> map toAlignment cs
                      let widths = case colspecs of
                                     [] -> replicate numrows ColWidthDefault
                                     cs -> let ws = map toWidth cs
                                           in case sequence ws of
                                                Just ws' -> let tot = sum ws'
                                                            in  ColWidth . (/ tot) <$> ws'
                                                Nothing  -> replicate numrows ColWidthDefault
                      let toRow = Row nullAttr . map simpleCell
                          toHeaderRow l = if null l then [] else [toRow l]
                      return $ table (simpleCaption $ plain capt)
                                     (zip aligns widths)
                                     (TableHead nullAttr $ toHeaderRow headrows)
                                     [TableBody nullAttr 0 [] $ map toRow bodyrows]
                                     (TableFoot nullAttr [])
         isEntry x  = named "entry" x || named "td" x || named "th" x
         parseRow = mapM (parseMixed plain . elContent) . filterChildren isEntry
         sect n = sectWith (attrValue "id" e,[],[]) n
         sectWith attr n = do
           isbook <- gets dbBook
           let n' = if isbook || n == 0 then n + 1 else n
           headerText <- case filterChild (named "title") e `mplus`
                              (filterChild (named "info") e >>=
                                  filterChild (named "title")) of
                            Just t  -> getInlines t
                            Nothing -> return mempty
           modify $ \st -> st{ dbSectionLevel = n }
           b <- getBlocks e
           modify $ \st -> st{ dbSectionLevel = n - 1 }
           return $ headerWith attr n' headerText <> b
         lineItems = mapM getInlines $ filterChildren (named "line") e
         -- | Admonitions are parsed into a div. Following other Docbook tools that output HTML,
         -- we parse the optional title as a div with the @title@ class, and give the
         -- block itself a class corresponding to the admonition name.
         parseAdmonition label = do
           -- <title> elements can be directly nested inside an admonition block, use
           -- it if it's there. It is unclear whether we should include the label in
           -- the title: docbook references are ambiguous on that, and some implementations of admonitions
           -- (e.g. asciidoctor) just use an icon in all cases. To be conservative, we don't
           -- include the label and leave it to styling.
           title <- case filterChild (named "title") e of
                        Just t  -> divWith ("", ["title"], []) . plain <$> getInlines t
                        Nothing -> return mempty
           -- this will ignore the title element if it is present
           b <- getBlocks e
           -- we also attach the label as a class, so it can be styled properly
           return $ divWith (attrValue "id" e,[label],[]) (title <> b)

getInlines :: PandocMonad m => Element -> DB m Inlines
getInlines e' = (trimInlines . mconcat) <$>
                 mapM parseInline (elContent e')

strContentRecursive :: Element -> String
strContentRecursive = strContent .
  (\e' -> e'{ elContent = map elementToStr $ elContent e' })

elementToStr :: Content -> Content
elementToStr (Elem e') = Text $ CData CDataText (strContentRecursive e') Nothing
elementToStr x = x

parseInline :: PandocMonad m => Content -> DB m Inlines
parseInline (Text (CData _ s _)) = return $ text $ T.pack s
parseInline (CRef ref) =
  return $ text $ maybe (T.toUpper $ T.pack ref) T.pack $ lookupEntity ref
parseInline (Elem e) =
  case qName (elName e) of
        "phrase" -> do
          let ident = attrValue "id" e
          let classes = T.words $ attrValue "class" e
          if ident /= "" || classes /= []
            then spanWith (ident,classes,[]) <$> innerInlines
            else innerInlines
        "equation" -> equation e displayMath
        "informalequation" -> equation e displayMath
        "inlineequation" -> equation e math
        "subscript" -> subscript <$> innerInlines
        "superscript" -> superscript <$> innerInlines
        "inlinemediaobject" -> getMediaobject e
        "quote" -> do
            qt <- gets dbQuoteType
            let qt' = if qt == SingleQuote then DoubleQuote else SingleQuote
            modify $ \st -> st{ dbQuoteType = qt' }
            contents <- innerInlines
            modify $ \st -> st{ dbQuoteType = qt }
            return $ if qt == SingleQuote
                        then singleQuoted contents
                        else doubleQuoted contents
        "simplelist" -> simpleList
        "segmentedlist" -> segmentedList
        "classname" -> codeWithLang
        "code" -> codeWithLang
        "filename" -> codeWithLang
        "envar" -> codeWithLang
        "literal" -> codeWithLang
        "computeroutput" -> codeWithLang
        "prompt" -> codeWithLang
        "parameter" -> codeWithLang
        "option" -> codeWithLang
        "optional" -> do x <- getInlines e
                         return $ str "[" <> x <> str "]"
        "replaceable" -> do x <- getInlines e
                            return $ str "<" <> x <> str ">"
        "markup" -> codeWithLang
        "wordasword" -> emph <$> innerInlines
        "command" -> codeWithLang
        "varname" -> codeWithLang
        "function" -> codeWithLang
        "type"    -> codeWithLang
        "symbol"  -> codeWithLang
        "constant" -> codeWithLang
        "userinput" -> codeWithLang
        "systemitem" -> codeWithLang
        "varargs" -> return $ code "(...)"
        "keycap" -> return (str $ T.pack $ strContent e)
        "keycombo" -> keycombo <$>
                         mapM parseInline (elContent e)
        "menuchoice" -> menuchoice <$>
                         mapM parseInline (
                                        filter isGuiMenu $ elContent e)
        "xref" -> do
            content <- dbContent <$> get
            let linkend = attrValue "linkend" e
            let title = case attrValue "endterm" e of
                            ""      -> maybe "???" xrefTitleByElem
                                         (findElementById linkend content)
                            endterm -> maybe "???" (T.pack . strContent)
                                         (findElementById endterm content)
            return $ link ("#" <> linkend) "" (text title)
        "email" -> return $ link ("mailto:" <> T.pack (strContent e)) ""
                          $ str $ T.pack $ strContent e
        "uri" -> return $ link (T.pack $ strContent e) "" $ str $ T.pack $ strContent e
        "ulink" -> link (attrValue "url" e) "" <$> innerInlines
        "link" -> do
             ils <- innerInlines
             let href = case findAttr (QName "href" (Just "http://www.w3.org/1999/xlink") Nothing) e of
                               Just h -> T.pack h
                               _      -> "#" <> attrValue "linkend" e
             let ils' = if ils == mempty then str href else ils
             let attr = (attrValue "id" e, T.words $ attrValue "role" e, [])
             return $ linkWith attr href "" ils'
        "foreignphrase" -> emph <$> innerInlines
        "emphasis" -> case attrValue "role" e of
                             "bold"          -> strong <$> innerInlines
                             "strong"        -> strong <$> innerInlines
                             "strikethrough" -> strikeout <$> innerInlines
                             _               -> emph <$> innerInlines
        "footnote" -> (note . mconcat) <$>
                         mapM parseBlock (elContent e)
        "title" -> return mempty
        "affiliation" -> skip
        -- Note: this isn't a real docbook tag; it's what we convert
        -- <?asciidor-br?> to in handleInstructions, above.  A kludge to
        -- work around xml-light's inability to parse an instruction.
        "br" -> return linebreak
        _          -> skip >> innerInlines
   where skip = do
           lift $ report $ IgnoredElement $ T.pack $ qName (elName e)
           return mempty

         innerInlines = (trimInlines . mconcat) <$>
                          mapM parseInline (elContent e)
         codeWithLang = do
           let classes' = case attrValue "language" e of
                               "" -> []
                               l  -> [l]
           return $ codeWith (attrValue "id" e,classes',[]) $ T.pack $ strContentRecursive e
         simpleList = (mconcat . intersperse (str "," <> space)) <$> mapM getInlines
                         (filterChildren (named "member") e)
         segmentedList = do
           tit <- maybe (return mempty) getInlines $ filterChild (named "title") e
           segtits <- mapM getInlines $ filterChildren (named "segtitle") e
           segitems <- mapM (mapM getInlines . filterChildren (named "seg"))
                          $ filterChildren (named "seglistitem") e
           let toSeg = mconcat . zipWith (\x y -> strong (x <> str ":") <> space <>
                                  y <> linebreak) segtits
           let segs = mconcat $ map toSeg segitems
           let tit' = if tit == mempty
                         then mempty
                         else strong tit <> linebreak
           return $ linebreak <> tit' <> segs
         keycombo = spanWith ("",["keycombo"],[]) .
                    mconcat . intersperse (str "+")
         menuchoice = spanWith ("",["menuchoice"],[]) .
                    mconcat . intersperse (text " > ")
         isGuiMenu (Elem x) = named "guimenu" x || named "guisubmenu" x ||
                              named "guimenuitem" x
         isGuiMenu _        = False

         findElementById idString content
            = asum [filterElement (\x -> attrValue "id" x == idString) el | Elem el <- content]

         -- Use the 'xreflabel' attribute for getting the title of a xref link;
         -- if there's no such attribute, employ some heuristics based on what
         -- docbook-xsl does.
         xrefTitleByElem el
             | not (T.null xrefLabel) = xrefLabel
             | otherwise              = case qName (elName el) of
                  "chapter"      -> descendantContent "title" el
                  "section"      -> descendantContent "title" el
                  "sect1"        -> descendantContent "title" el
                  "sect2"        -> descendantContent "title" el
                  "sect3"        -> descendantContent "title" el
                  "sect4"        -> descendantContent "title" el
                  "sect5"        -> descendantContent "title" el
                  "cmdsynopsis"  -> descendantContent "command" el
                  "funcsynopsis" -> descendantContent "function" el
                  _              -> T.pack $ qName (elName el) ++ "_title"
          where
            xrefLabel = attrValue "xreflabel" el
            descendantContent name = maybe "???" (T.pack . strContent)
                                   . filterElementName (\n -> qName n == name)

-- | Extract a math equation from an element
--
-- asciidoc can generate Latex math in CDATA sections.
--
-- Note that if some MathML can't be parsed it is silently ignored!
equation
  :: Monad m
  => Element
  -- ^ The element from which to extract a mathematical equation
  -> (Text -> Inlines)
  -- ^ A constructor for some Inlines, taking the TeX code as input
  -> m Inlines
equation e constructor =
  return $ mconcat $ map constructor $ mathMLEquations <> latexEquations
  where
    mathMLEquations :: [Text]
    mathMLEquations = map writeTeX $ rights $ readMath
      (\x -> qName (elName x) == "math" && qPrefix (elName x) == Just "mml")
      (readMathML . T.pack . showElement)

    latexEquations :: [Text]
    latexEquations = readMath (\x -> qName (elName x) == "mathphrase")
                              (T.concat . fmap showVerbatimCData . elContent)

    readMath :: (Element -> Bool) -> (Element -> b) -> [b]
    readMath childPredicate fromElement =
      map (fromElement . everywhere (mkT removePrefix))
      $ filterChildren childPredicate e

-- | Get the actual text stored in a CData block. 'showContent'
-- returns the text still surrounded by the [[CDATA]] tags.
showVerbatimCData :: Content -> Text
showVerbatimCData (Text (CData _ d _)) = T.pack d
showVerbatimCData c = T.pack $ showContent c


-- | Set the prefix of a name to 'Nothing'
removePrefix :: QName -> QName
removePrefix elname = elname { qPrefix = Nothing }
