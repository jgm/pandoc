module Text.Pandoc.Readers.DocBook ( readDocBook ) where
import Text.Pandoc.Parsing (ParserState(..))
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.XML.Light
import Data.Monoid
import Data.Char (isSpace)
import Control.Monad.State
import Control.Applicative ((<$>))

{-

List of all DocBook tags, with [x] indicating implemented:

[ ] abbrev - An abbreviation, especially one followed by a period
[x] abstract - A summary
[ ] accel - A graphical user interface (GUI) keyboard shortcut
[ ] ackno - Acknowledgements in an Article
[ ] acronym - An often pronounceable word made from the initial
[ ] action - A response to a user event
[ ] address - A real-world address, generally a postal address
[ ] affiliation - The institutional affiliation of an individual
[ ] alt - Text representation for a graphical element
[ ] anchor - A spot in the document
[ ] answer - An answer to a question posed in a QandASet
[ ] appendix - An appendix in a Book or Article
[ ] appendixinfo - Meta-information for an Appendix
[ ] application - The name of a software program
[ ] area - A region defined for a Callout in a graphic or code example
[ ] areaset - A set of related areas in a graphic or code example
[ ] areaspec - A collection of regions in a graphic or code example
[ ] arg - An argument in a CmdSynopsis
[ ] article - An article
[x] articleinfo - Meta-information for an Article
[ ] artpagenums - The page numbers of an article as published
[ ] attribution - The source of a block quote or epigraph
[ ] audiodata - Pointer to external audio data
[ ] audioobject - A wrapper for audio data and its associated meta-information
[x] author - The name of an individual author
[ ] authorblurb - A short description or note about an author
[ ] authorgroup - Wrapper for author information when a document has
    multiple authors or collabarators
[ ] authorinitials - The initials or other short identifier for an author
[ ] beginpage - The location of a page break in a print version of the document
[ ] bibliocoverage - The spatial or temporal coverage of a document
[ ] bibliodiv - A section of a Bibliography
[ ] biblioentry - An entry in a Bibliography
[ ] bibliography - A bibliography
[ ] bibliographyinfo - Meta-information for a Bibliography
[ ] biblioid - An identifier for a document
[ ] bibliolist - A wrapper for a set of bibliography entries
[ ] bibliomisc - Untyped bibliographic information
[ ] bibliomixed - An entry in a Bibliography
[ ] bibliomset - A cooked container for related bibliographic information
[ ] biblioref - A cross reference to a bibliographic entry
[ ] bibliorelation - The relationship of a document to another
[ ] biblioset - A raw container for related bibliographic information
[ ] bibliosource - The source of a document
[ ] blockinfo - Meta-information for a block element
[x] blockquote - A quotation set off from the main text
[ ] book - A book
[ ] bookinfo - Meta-information for a Book
[ ] bridgehead - A free-floating heading
[ ] callout - A “called out” description of a marked Area
[ ] calloutlist - A list of Callouts
[ ] caption - A caption
[ ] caution - A note of caution
[ ] chapter - A chapter, as of a book
[ ] chapterinfo - Meta-information for a Chapter
[ ] citation - An inline bibliographic reference to another published work
[ ] citebiblioid - A citation of a bibliographic identifier
[ ] citerefentry - A citation to a reference page
[ ] citetitle - The title of a cited work
[ ] city - The name of a city in an address
[ ] classname - The name of a class, in the object-oriented programming sense
[ ] classsynopsis - The syntax summary for a class definition
[ ] classsynopsisinfo - Information supplementing the contents of
    a ClassSynopsis
[ ] cmdsynopsis - A syntax summary for a software command
[ ] co - The location of a callout embedded in text
[x] code - An inline code fragment
[ ] col - Specifications for a column in an HTML table
[ ] colgroup - A group of columns in an HTML table
[ ] collab - Identifies a collaborator
[ ] collabname - The name of a collaborator
[ ] colophon - Text at the back of a book describing facts about its production
[ ] colspec - Specifications for a column in a table
[ ] command - The name of an executable program or other software command
[ ] computeroutput - Data, generally text, displayed or presented by a computer
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
[ ] email - An email address
[x] emphasis - Emphasized text
[ ] entry - A cell in a table
[ ] entrytbl - A subtable appearing in place of an Entry in a table
[ ] envar - A software environment variable
[ ] epigraph - A short inscription at the beginning of a document or component
[ ] equation - A displayed mathematical equation
[ ] errorcode - An error code
[ ] errorname - An error name
[ ] errortext - An error message.
[ ] errortype - The classification of an error message
[ ] example - A formal example, with a title
[ ] exceptionname - The name of an exception
[ ] fax - A fax number
[ ] fieldsynopsis - The name of a field in a class definition
[ ] figure - A formal figure, generally an illustration, with a title
[ ] filename - The name of a file
[ ] firstname - The first name of a person
[ ] firstterm - The first occurrence of a term
[ ] footnote - A footnote
[ ] footnoteref - A cross reference to a footnote (a footnote mark)
[ ] foreignphrase - A word or phrase in a language other than the primary
    language of the document
[ ] formalpara - A paragraph with a title
[ ] funcdef - A function (subroutine) name and its return type
[ ] funcparams - Parameters for a function referenced through a function
    pointer in a synopsis
[ ] funcprototype - The prototype of a function
[ ] funcsynopsis - The syntax summary for a function definition
[ ] funcsynopsisinfo - Information supplementing the FuncDefs of a FuncSynopsis
[x] function - The name of a function or subroutine, as in a
    programming language
[ ] glossary - A glossary
[ ] glossaryinfo - Meta-information for a Glossary
[ ] glossdef - A definition in a GlossEntry
[ ] glossdiv - A division in a Glossary
[ ] glossentry - An entry in a Glossary or GlossList
[ ] glosslist - A wrapper for a set of GlossEntrys
[ ] glosssee - A cross-reference from one GlossEntry to another
[ ] glossseealso - A cross-reference from one GlossEntry to another
[ ] glossterm - A glossary term
[ ] graphic - A displayed graphical object (not an inline)
[ ] graphicco - A graphic that contains callout areas
[ ] group - A group of elements in a CmdSynopsis
[ ] guibutton - The text on a button in a GUI
[ ] guiicon - Graphic and/or text appearing as a icon in a GUI
[ ] guilabel - The text of a label in a GUI
[ ] guimenu - The name of a menu in a GUI
[ ] guimenuitem - The name of a terminal menu item in a GUI
[ ] guisubmenu - The name of a submenu in a GUI
[ ] hardware - A physical part of a computer system
[ ] highlights - A summary of the main points of the discussed component
[ ] holder - The name of the individual or organization that holds a copyright
[ ] honorific - The title of a person
[ ] html:form - An HTML form
[ ] imagedata - Pointer to external image data
[ ] imageobject - A wrapper for image data and its associated meta-information
[ ] imageobjectco - A wrapper for an image object with callouts
[ ] important - An admonition set off from the text
[ ] index - An index
[ ] indexdiv - A division in an index
[ ] indexentry - An entry in an index
[ ] indexinfo - Meta-information for an Index
[ ] indexterm - A wrapper for terms to be indexed
[ ] info - A wrapper for information about a component or other block. (DocBook v5)
[ ] informalequation - A displayed mathematical equation without a title
[ ] informalexample - A displayed example without a title
[ ] informalfigure - A untitled figure
[ ] informaltable - A table without a title
[ ] initializer - The initializer for a FieldSynopsis
[ ] inlineequation - A mathematical equation or expression occurring inline
[ ] inlinegraphic - An object containing or pointing to graphical data
    that will be rendered inline
[ ] inlinemediaobject - An inline media object (video, audio, image, and so on)
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
[ ] keycap - The text printed on a key on a keyboard
[ ] keycode - The internal, frequently numeric, identifier for a key
    on a keyboard
[ ] keycombo - A combination of input actions
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
[ ] listitem - A wrapper for the elements of a list item
[x] literal - Inline text that is some literal value
[ ] literallayout - A block of text in which line breaks and white space are
    to be reproduced faithfully
[ ] lot - A list of the titles of formal objects (as tables or figures) in
    a document
[ ] lotentry - An entry in a list of titles
[ ] manvolnum - A reference volume number
[ ] markup - A string of formatting markup in text that is to be
    represented literally
[ ] mathphrase - A mathematical phrase, an expression that can be represented
    with ordinary text and a small amount of markup
[ ] medialabel - A name that identifies the physical medium on which some
    information resides
[ ] mediaobject - A displayed media object (video, audio, image, etc.)
[ ] mediaobjectco - A media object that contains callouts
[ ] member - An element of a simple list
[ ] menuchoice - A selection or series of selections from a menu
[ ] methodname - The name of a method
[ ] methodparam - Parameters to a method
[ ] methodsynopsis - A syntax summary for a method
[ ] mml:math - A MathML equation
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
[ ] note - A message set off from the text
[ ] objectinfo - Meta-information for an object
[ ] olink - A link that addresses its target indirectly, through an entity
[ ] ooclass - A class in an object-oriented programming language
[ ] ooexception - An exception in an object-oriented programming language
[ ] oointerface - An interface in an object-oriented programming language
[ ] option - An option for a software command
[ ] optional - Optional information
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
[ ] parameter - A value or a symbolic reference to a value
[ ] part - A division in a book
[ ] partinfo - Meta-information for a Part
[ ] partintro - An introduction to the contents of a part
[ ] personblurb - A short description or note about a person
[ ] personname - The personal name of an individual
[ ] phone - A telephone number
[ ] phrase - A span of text
[ ] pob - A post office box in an address
[ ] postcode - A postal code in an address
[ ] preface - Introductory matter preceding the first chapter of a book
[ ] prefaceinfo - Meta-information for a Preface
[ ] primary - The primary word or phrase under which an index term should be
    sorted
[ ] primaryie - A primary term in an index entry, not in the text
[ ] printhistory - The printing history of a document
[ ] procedure - A list of operations to be performed in a well-defined sequence
[ ] production - A production in a set of EBNF productions
[ ] productionrecap - A cross-reference to an EBNF production
[ ] productionset - A set of EBNF productions
[ ] productname - The formal name of a product
[ ] productnumber - A number assigned to a product
[ ] programlisting - A literal listing of all or part of a program
[ ] programlistingco - A program listing with associated areas used in callouts
[ ] prompt - A character or string indicating the start of an input field in
    a computer display
[ ] property - A unit of data associated with some part of a computer system
[ ] pubdate - The date of publication of a document
[ ] publisher - The publisher of a document
[ ] publishername - The name of the publisher of a document
[ ] pubsnumber - A number assigned to a publication other than an ISBN or ISSN
    or inventory part number
[ ] qandadiv - A titled division in a QandASet
[ ] qandaentry - A question/answer set within a QandASet
[ ] qandaset - A question-and-answer set
[ ] question - A question in a QandASet
[ ] quote - An inline quotation
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
[ ] refsect1 - A major subsection of a reference entry
[ ] refsect1info - Meta-information for a RefSect1
[ ] refsect2 - A subsection of a RefSect1
[ ] refsect2info - Meta-information for a RefSect2
[ ] refsect3 - A subsection of a RefSect2
[ ] refsect3info - Meta-information for a RefSect3
[ ] refsection - A recursive section in a refentry
[ ] refsectioninfo - Meta-information for a refsection
[ ] refsynopsisdiv - A syntactic synopsis of the subject of the reference page
[ ] refsynopsisdivinfo - Meta-information for a RefSynopsisDiv
[ ] releaseinfo - Information about a particular release of a document
[ ] remark - A remark (or comment) intended for presentation in a draft
    manuscript
[ ] replaceable - Content that may or must be replaced by the user
[ ] returnvalue - The value returned by a function
[ ] revdescription - A extended description of a revision to a document
[ ] revhistory - A history of the revisions to a document
[ ] revision - An entry describing a single revision in the history of the
    revisions to a document
[ ] revnumber - A document revision number
[ ] revremark - A description of a revision to a document
[ ] rhs - The right-hand side of an EBNF production
[ ] row - A row in a table
[ ] sbr - An explicit line break in a command synopsis
[ ] screen - Text that a user sees or might see on a computer screen
[ ] screenco - A screen with associated areas used in callouts
[ ] screeninfo - Information about how a screen shot was produced
[ ] screenshot - A representation of what the user sees or might see on a
    computer screen
[ ] secondary - A secondary word or phrase in an index term
[ ] secondaryie - A secondary term in an index entry, rather than in the text
[x] sect1 - A top-level section of document
[ ] sect1info - Meta-information for a Sect1
[x] sect2 - A subsection within a Sect1
[ ] sect2info - Meta-information for a Sect2
[x] sect3 - A subsection within a Sect2
[ ] sect3info - Meta-information for a Sect3
[x] sect4 - A subsection within a Sect3
[ ] sect4info - Meta-information for a Sect4
[x] sect5 - A subsection within a Sect4
[ ] sect5info - Meta-information for a Sect5
[x] section - A recursive section
[ ] sectioninfo - Meta-information for a recursive section
[ ] see - Part of an index term directing the reader instead to another entry
    in the index
[ ] seealso - Part of an index term directing the reader also to another entry
    in the index
[ ] seealsoie - A See also entry in an index, rather than in the text
[ ] seeie - A See entry in an index, rather than in the text
[ ] seg - An element of a list item in a segmented list
[ ] seglistitem - A list item in a segmented list
[ ] segmentedlist - A segmented list, a list of sets of elements
[ ] segtitle - The title of an element of a list item in a segmented list
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
[ ] simpara - A paragraph that contains only text and inline markup, no block
    elements
[ ] simplelist - An undecorated list of single words or short phrases
[ ] simplemsgentry - A wrapper for a simpler entry in a message set
[ ] simplesect - A section of a document with no subdivisions
[ ] spanspec - Formatting information for a spanned column in a table
[ ] state - A state or province in an address
[ ] step - A unit of action in a procedure
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
[ ] substeps - A wrapper for steps that occur within steps in a procedure
[ ] subtitle - The subtitle of a document
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
[ ] systemitem - A system-related item or term
[ ] table - A formal table in a document
[ ] task - A task to be completed
[ ] taskprerequisites - The prerequisites for a task
[ ] taskrelated - Information related to a task
[ ] tasksummary - A summary of a task
[ ] tbody - A wrapper for the rows of a table or informal table
[ ] td - A table entry in an HTML table
[ ] term - The word or phrase being defined or described in a variable list
[ ] termdef - An inline term definition
[ ] tertiary - A tertiary word or phrase in an index term
[ ] tertiaryie - A tertiary term in an index entry, rather than in the text
[ ] textdata - Pointer to external text data
[ ] textobject - A wrapper for a text description of an object and its
    associated meta-information
[ ] tfoot - A table footer consisting of one or more rows
[ ] tgroup - A wrapper for the main content of a table, or part of a table
[ ] th - A table header entry in an HTML table
[ ] thead - A table header consisting of one or more rows
[ ] tip - A suggestion to the user, set off from the text
[x] title - The text of the title of a section of a document or of a formal
    block-level element
[ ] titleabbrev - The abbreviation of a Title
[ ] toc - A table of contents
[ ] tocback - An entry in a table of contents for a back matter component
[ ] tocchap - An entry in a table of contents for a component in the body of
    a document
[ ] tocentry - A component title in a table of contents
[ ] tocfront - An entry in a table of contents for a front matter component
[ ] toclevel1 - A top-level entry within a table of contents entry for a
    chapter-like component
[ ] toclevel2 - A second-level entry within a table of contents entry for a 
    chapter-like component
[ ] toclevel3 - A third-level entry within a table of contents entry for a 
    chapter-like component
[ ] toclevel4 - A fourth-level entry within a table of contents entry for a 
    chapter-like component
[ ] toclevel5 - A fifth-level entry within a table of contents entry for a 
    chapter-like component
[ ] tocpart - An entry in a table of contents for a part of a book
[ ] token - A unit of information
[ ] tr - A row in an HTML table
[ ] trademark - A trademark
[ ] type - The classification of a value
[x] ulink - A link that addresses its target by means of a URL
    (Uniform Resource Locator)
[ ] uri - A Uniform Resource Identifier
[ ] userinput - Data entered by the user
[x] varargs - An empty element in a function synopsis indicating a variable
    number of arguments
[ ] variablelist - A list in which each entry is composed of a set of one or
    more terms and an associated description
[ ] varlistentry - A wrapper for a set of terms and the associated description
    in a variable list
[x] varname - The name of a variable
[ ] videodata - Pointer to external video data
[ ] videoobject - A wrapper for video data and its associated meta-information
[ ] void - An empty element in a function synopsis indicating that the
    function in question takes no arguments
[ ] volumenum - The volume number of a document in a set (as of books in a set
    or articles in a journal)
[ ] warning - An admonition set off from the text
[ ] wordasword - A word meant specifically as a word and not representing
    anything else
[ ] xref - A cross reference to another part of the document
[ ] year - The year of publication of a document

-}

type DB = State DBState

data DBState = DBState{ dbSectionLevel :: Int
                      , dbQuoteType    :: QuoteType
                      , dbDocTitle     :: Inlines
                      , dbDocAuthors   :: [Inlines]
                      , dbDocDate      :: Inlines
                      } deriving Show

readDocBook :: ParserState -> String -> Pandoc
readDocBook st inp = setTitle (dbDocTitle st')
                   $ setAuthors (dbDocAuthors st')
                   $ setDate (dbDocDate st')
                   $ doc $ mconcat bs
  where (bs, st') = runState (mapM parseBlock $ parseXML inp)
                             DBState{ dbSectionLevel = 0
                                    , dbQuoteType = DoubleQuote
                                    , dbDocTitle = mempty
                                    , dbDocAuthors = []
                                    , dbDocDate = mempty
                                    }

-- convenience function to get an attribute value, defaulting to ""
attrValue :: String -> Element -> String
attrValue attr elt =
  case lookupAttrBy (\x -> qName x == attr) (elAttribs elt) of
    Just z  -> z
    Nothing -> ""

parseBlock :: Content -> DB Blocks
parseBlock (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock (Text (CData _ s _)) = if all isSpace s
                                     then return mempty
                                     else return $ plain $ text s
parseBlock (Elem e) =
  case qName (elName e) of
        "para"  -> para <$> getInlines e
        "blockquote" -> blockQuote <$> getBlocks e
        "sect1" -> sect 1
        "sect2" -> sect 2
        "sect3" -> sect 3
        "sect4" -> sect 4
        "sect5" -> sect 5
        "section" -> gets dbSectionLevel >>= sect . (+1)
        "abstract" -> blockQuote <$> getBlocks e
        "itemizedlist" -> bulletList <$> listitems
        "orderedlist" -> orderedList <$> listitems -- TODO list attributes
        "info" -> getTitle >> getAuthors >> getDate >> return mempty
        "articleinfo" -> getTitle >> getAuthors >> getDate >> return mempty
        "programlisting" -> return $ codeBlock $ strContent e  -- TODO attrs
        "?xml"  -> return mempty
        _       -> getBlocks e
   where getBlocks e' =  mconcat <$> (mapM parseBlock $ elContent e')
         getInlines e' = (trimInlines . mconcat) <$>
                       (mapM parseInline $ elContent e')
         isTitle e' = qName (elName e') == "title"
         skipWhite (Text (CData _ s _):xs) | all isSpace s = skipWhite xs
                                           | otherwise     = xs
         skipWhite xs = xs
         listitems = mapM getBlocks $ findChildren (unqual "listitem") e
         getTitle = case findChild (unqual "title") e of
                         Just t  -> do
                            tit <- getInlines t
                            modify $ \st -> st{dbDocTitle = tit}
                         Nothing -> return ()
         getAuthors = do
                      auths <- mapM getInlines
                               $ findChildren (unqual "author") e
                      modify $ \st -> st{dbDocAuthors = auths}
         getDate = case findChild (unqual "date") e of
                         Just t  -> do
                            dat <- getInlines t
                            modify $ \st -> st{dbDocDate = dat}
                         Nothing -> return ()
         sect n = case skipWhite (elContent e) of
                        ((Elem t):body)
                          | isTitle t -> do
                                  h <- header n <$> (getInlines t)
                                  modify $ \st -> st{ dbSectionLevel = n }
                                  b <- mconcat <$> (mapM parseBlock body)
                                  modify $ \st -> st{ dbSectionLevel = n - 1 }
                                  return $ h <> b
                        body      -> do
                                  let h = header n mempty
                                  modify $ \st -> st{ dbSectionLevel = n }
                                  b <- mconcat <$> (mapM parseBlock body)
                                  modify $ \st -> st{ dbSectionLevel = n - 1 }
                                  return $ h <> b
parseBlock (CRef _) = return mempty

parseInline :: Content -> DB Inlines
parseInline (Text (CData _ s _)) = return $ text s
parseInline (Elem e) =
  case qName (elName e) of
        "subscript" -> subscript <$> innerInlines
        "superscript" -> superscript <$> innerInlines
        "quote" -> do
            qt <- gets dbQuoteType
            let qt' = if qt == SingleQuote then DoubleQuote else SingleQuote
            modify $ \st -> st{ dbQuoteType = qt' }
            contents <- innerInlines
            modify $ \st -> st{ dbQuoteType = qt }
            return $ if qt == SingleQuote
                        then singleQuoted contents
                        else doubleQuoted contents
        "code" -> return $ code $ strContent e -- TODO attrs
        "literal" -> return $ code $ strContent e -- TODO attrs
        "varname" -> return $ codeWith ("",["varname"],[]) $ strContent e
        "function" -> return $ codeWith ("",["function"],[]) $ strContent e
        "type"    -> return $ codeWith ("",["type"],[]) $ strContent e
        "symbol"  -> return $ codeWith ("",["symbol"],[]) $ strContent e
        "constant" -> return $ codeWith ("",["constant"],[]) $ strContent e
        "userinput" -> return $ codeWith ("",["userinput"],[]) $ strContent e
        "varargs" -> return $ str "(…)"
        "ulink" -> link (attrValue "url" e) "" <$> innerInlines
        "link" -> case findAttr (QName "href" Nothing $ Just "xlink") e of
                       Just href -> link href "" <$> innerInlines
                       _         -> link ('#' : attrValue "linkend" e) ""
                                      <$> innerInlines
        "emphasis" -> case attrValue "role" e of
                             "strong" -> strong <$> innerInlines
                             _        -> emph <$> innerInlines
        "footnote" -> (note . mconcat) <$> (mapM parseBlock $ elContent e)
        _          -> innerInlines
   where innerInlines = (trimInlines . mconcat) <$>
                          (mapM parseInline $ elContent e)
parseInline (CRef _) = return mempty
