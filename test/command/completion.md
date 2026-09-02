```
% pandoc --completion=bash
^D
# This script enables bash autocompletion for pandoc.  To enable
# bash completion, add this to your .bashrc:
# eval "$(pandoc --completion=bash)"

_pandoc()
{
    local cur prev opts informats outformats highlight_styles math_methods datafiles
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # These should be filled in by pandoc:
    opts="-f -r --from --read -t -w --to --write -o --output --data-dir -M --metadata --metadata-file -d --defaults --file-scope --sandbox -s --standalone --template -V --variable --variable-json --wrap --ascii --toc --table-of-contents --toc-depth --lof --list-of-figures --lot --list-of-tables -N --number-sections --number-offset --top-level-division --extract-media --resource-path -H --include-in-header -B --include-before-body -A --include-after-body --no-highlight --highlight-style --syntax-definition --syntax-highlighting --dpi --eol --columns -p --preserve-tabs --tab-stop --pdf-engine --pdf-engine-opt --reference-doc --self-contained --embed-resources --link-images --request-header --no-check-certificate --abbreviations --typst-input --indented-code-classes --default-image-extension -F --filter -L --lua-filter --shift-heading-level-by --base-header-level --track-changes --strip-comments --reference-links --reference-location --figure-caption-position --table-caption-position --markdown-headings --list-tables --listings -i --incremental --slide-level --section-divs --html-q-tags --email-obfuscation --id-prefix -T --title-prefix -c --css --epub-subdirectory --epub-cover-image --epub-title-page --epub-metadata --epub-embed-font --split-level --chunk-template --epub-chapter-level --ipynb-output -C --citeproc --bibliography --csl --citation-abbreviations --natbib --biblatex --math-method --mathml --webtex --mathjax --katex --gladtex --trace --dump-args --ignore-args --verbose --quiet --fail-if-warnings --log --completion --bash-completion --list-input-formats --list-output-formats --list-extensions --list-highlight-languages --list-highlight-styles -D --print-default-template --print-default-data-file --print-highlight-style -v --version -h --help"
    informats="asciidoc biblatex bibtex bits commonmark commonmark_x creole csljson csv djot docbook docx dokuwiki endnotexml epub fb2 gfm haddock html ipynb jats jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict mdoc mediawiki muse native odt opml org pod pptx ris rst rtf t2t textile tikiwiki tsv twiki typst vimwiki xlsx xml"
    outformats="ansi asciidoc asciidoc_legacy asciidoctor bbcode bbcode_fluxbb bbcode_hubzilla bbcode_phpbb bbcode_steam bbcode_xenforo beamer biblatex bibtex chunkedhtml commonmark commonmark_x context csljson djot docbook docbook4 docbook5 docx dokuwiki dzslides epub epub2 epub3 fb2 gfm haddock html html4 html5 icml ipynb jats jats_archiving jats_articleauthoring jats_publishing jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict markua mediawiki ms muse native odt opendocument opml org pdf plain pptx revealjs rst rtf s5 slideous slidy t2t tei texinfo textile typst vimdoc xml xwiki zimwiki"
    highlight_styles="pygments tango espresso zenburn kate monochrome breezedark haddock"
    math_methods="plain mathml webtex mathjax katex gladtex"
    datafiles="MANUAL.txt abbreviations creole.lua default.csl docbook-entities.txt docx/[Content_Types].xml docx/_rels/.rels docx/docProps/app.xml docx/docProps/core.xml docx/docProps/custom.xml docx/word/_rels/document.xml.rels docx/word/_rels/footnotes.xml.rels docx/word/comments.xml docx/word/document.xml docx/word/fontTable.xml docx/word/footnotes.xml docx/word/numbering.xml docx/word/settings.xml docx/word/styles.xml docx/word/theme/theme1.xml docx/word/webSettings.xml dzslides/template.html epub.css init.lua odt/META-INF/manifest.xml odt/content.xml odt/manifest.rdf odt/meta.xml odt/mimetype odt/styles.xml pptx/[Content_Types].xml pptx/_rels/.rels pptx/docProps/app.xml pptx/docProps/core.xml pptx/ppt/_rels/presentation.xml.rels pptx/ppt/notesMasters/_rels/notesMaster1.xml.rels pptx/ppt/notesMasters/notesMaster1.xml pptx/ppt/notesSlides/_rels/notesSlide1.xml.rels pptx/ppt/notesSlides/_rels/notesSlide2.xml.rels pptx/ppt/notesSlides/notesSlide1.xml pptx/ppt/notesSlides/notesSlide2.xml pptx/ppt/presProps.xml pptx/ppt/presentation.xml pptx/ppt/slideLayouts/_rels/slideLayout1.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout10.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout11.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout2.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout3.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout4.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout5.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout6.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout7.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout8.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout9.xml.rels pptx/ppt/slideLayouts/slideLayout1.xml pptx/ppt/slideLayouts/slideLayout10.xml pptx/ppt/slideLayouts/slideLayout11.xml pptx/ppt/slideLayouts/slideLayout2.xml pptx/ppt/slideLayouts/slideLayout3.xml pptx/ppt/slideLayouts/slideLayout4.xml pptx/ppt/slideLayouts/slideLayout5.xml pptx/ppt/slideLayouts/slideLayout6.xml pptx/ppt/slideLayouts/slideLayout7.xml pptx/ppt/slideLayouts/slideLayout8.xml pptx/ppt/slideLayouts/slideLayout9.xml pptx/ppt/slideMasters/_rels/slideMaster1.xml.rels pptx/ppt/slideMasters/slideMaster1.xml pptx/ppt/slides/_rels/slide1.xml.rels pptx/ppt/slides/_rels/slide2.xml.rels pptx/ppt/slides/_rels/slide3.xml.rels pptx/ppt/slides/_rels/slide4.xml.rels pptx/ppt/slides/slide1.xml pptx/ppt/slides/slide2.xml pptx/ppt/slides/slide3.xml pptx/ppt/slides/slide4.xml pptx/ppt/tableStyles.xml pptx/ppt/theme/theme1.xml pptx/ppt/theme/theme2.xml pptx/ppt/viewProps.xml reference.docx reference.odt reference.pptx templates/affiliations.jats templates/after-header-includes.latex templates/article.jats_publishing templates/common.latex templates/default.ansi templates/default.asciidoc templates/default.bbcode templates/default.beamer templates/default.biblatex templates/default.bibtex templates/default.chunkedhtml templates/default.commonmark templates/default.context templates/default.djot templates/default.docbook4 templates/default.docbook5 templates/default.dokuwiki templates/default.dzslides templates/default.epub2 templates/default.epub3 templates/default.haddock templates/default.html4 templates/default.html5 templates/default.icml templates/default.jats_archiving templates/default.jats_articleauthoring templates/default.jats_publishing templates/default.jira templates/default.latex templates/default.man templates/default.markdown templates/default.markua templates/default.mediawiki templates/default.ms templates/default.muse templates/default.opendocument templates/default.openxml templates/default.opml templates/default.org templates/default.plain templates/default.revealjs templates/default.rst templates/default.rtf templates/default.s5 templates/default.slideous templates/default.slidy templates/default.t2t templates/default.tei templates/default.texinfo templates/default.textile templates/default.typst templates/default.vimdoc templates/default.xwiki templates/default.zimwiki templates/document-metadata.latex templates/font-settings.latex templates/fonts.latex templates/hypersetup.latex templates/passoptions.latex templates/styles.citations.html templates/styles.html templates/template.typst translations/af.yaml translations/alt.yaml translations/am.yaml translations/ar.yaml translations/as.yaml translations/ast.yaml translations/az.yaml translations/be.yaml translations/bg.yaml translations/bn.yaml translations/bo.yaml translations/br.yaml translations/bs.yaml translations/bua.yaml translations/ca.yaml translations/ckb-Arab.yaml translations/ckb-Latn.yaml translations/cs.yaml translations/cu.yaml translations/cy.yaml translations/cz.yaml translations/da.yaml translations/de.yaml translations/dsb.yaml translations/el.yaml translations/en.yaml translations/eo.yaml translations/es-ES.yaml translations/es-MX.yaml translations/es.yaml translations/et.yaml translations/eu.yaml translations/fa.yaml translations/fi.yaml translations/fil.yaml translations/fr.yaml translations/fur.yaml translations/ga.yaml translations/gd.yaml translations/gl.yaml translations/grc.yaml translations/gu.yaml translations/ha.yaml translations/he.yaml translations/hi.yaml translations/hr.yaml translations/hsb.yaml translations/hu.yaml translations/hy.yaml translations/ia.yaml translations/id.yaml translations/is.yaml translations/it.yaml translations/ja.yaml translations/ka.yaml translations/km.yaml translations/kmr-Arab.yaml translations/kmr-Latn.yaml translations/kn.yaml translations/ko.yaml translations/la.yaml translations/lb.yaml translations/lo.yaml translations/lt.yaml translations/lv.yaml translations/mk.yaml translations/ml.yaml translations/mn.yaml translations/mr.yaml translations/ms.yaml translations/nb.yaml translations/nko.yaml translations/nl.yaml translations/nn.yaml translations/no.yaml translations/oc.yaml translations/or.yaml translations/pa.yaml translations/pl.yaml translations/pms.yaml translations/pt-BR.yaml translations/pt-PT.yaml translations/pt.yaml translations/rm.yaml translations/ro.yaml translations/ru.yaml translations/se.yaml translations/si.yaml translations/sk.yaml translations/sl.yaml translations/sq.yaml translations/sr-Cyrl.yaml translations/sr-Latn.yaml translations/sr.yaml translations/sv.yaml translations/ta.yaml translations/te.yaml translations/th.yaml translations/tk.yaml translations/tr.yaml translations/ua.yaml translations/ug.yaml translations/uk.yaml translations/ur.yaml translations/vi.yaml translations/zh-Hans.yaml translations/zh-Hant.yaml"

    case "${prev}" in
         -f|-r|--from|--read)
             COMPREPLY=( $(compgen -W "${informats}" -- ${cur}) )
             return 0
             ;;
         -t|-w|--to|--write|-D|--print-default-template)
             COMPREPLY=( $(compgen -W "${outformats}" -- ${cur}) )
             return 0
             ;;
         --wrap)
             COMPREPLY=( $(compgen -W "auto none preserve" -- ${cur}) )
             return 0
             ;;
         --top-level-division)
             COMPREPLY=( $(compgen -W "section chapter part" -- ${cur}) )
             return 0
             ;;
         --highlight-style|--print-highlight-style)
             COMPREPLY=( $(compgen -W "${highlight_styles}" -- ${cur}) )
             return 0
             ;;
         --syntax-highlighting)
             COMPREPLY=( $(compgen -W "none default idiomatic" -- ${cur}) )
             return 0
             ;;
         --eol)
             COMPREPLY=( $(compgen -W "crlf lf native" -- ${cur}) )
             return 0
             ;;
         --pdf-engine)
             COMPREPLY=( $(compgen -W "weasyprint wkhtmltopdf pagedjs-cli prince pdflatex lualatex xelatex latexmk tectonic pdflatex-dev lualatex-dev groff pdfroff typst context" -- ${cur}) )
             return 0
             ;;
         --track-changes)
             COMPREPLY=( $(compgen -W "accept reject all" -- ${cur}) )
             return 0
             ;;
         --reference-location)
             COMPREPLY=( $(compgen -W "block section document" -- ${cur}) )
             return 0
             ;;
         --figure-caption-position|--table-caption-position)
             COMPREPLY=( $(compgen -W "above below" -- ${cur}) )
             return 0
             ;;
         --markdown-headings)
             COMPREPLY=( $(compgen -W "setext atx" -- ${cur}) )
             return 0
             ;;
         --email-obfuscation)
             COMPREPLY=( $(compgen -W "references javascript none" -- ${cur}) )
             return 0
             ;;
         --ipynb-output)
             COMPREPLY=( $(compgen -W "all none best" -- ${cur}) )
             return 0
             ;;
         --math-method)
             COMPREPLY=( $(compgen -W "${math_methods}" -- ${cur}) )
             return 0
             ;;
         --print-default-data-file)
             COMPREPLY=( $(compgen -W "${datafiles}" -- ${cur}) )
             return 0
             ;;
         *)
             ;;
    esac

    case "${cur}" in
         -*)
             COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
             return 0
             ;;
         *)
             local IFS=$'\n'
             COMPREPLY=( $(compgen -X '' -f "${cur}") )
             return 0
             ;;
    esac

}

complete -o filenames -o bashdefault -F _pandoc pandoc

.
```

```
% pandoc --completion=zsh
^D
#compdef pandoc

_pandoc() {
  local -a args
  args=(
    '-f[Reader format]:FORMAT:(asciidoc biblatex bibtex bits commonmark commonmark_x creole csljson csv djot docbook docx dokuwiki endnotexml epub fb2 gfm haddock html ipynb jats jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict mdoc mediawiki muse native odt opml org pod pptx ris rst rtf t2t textile tikiwiki tsv twiki typst vimwiki xlsx xml)'
    '-r[Reader format]:FORMAT:(asciidoc biblatex bibtex bits commonmark commonmark_x creole csljson csv djot docbook docx dokuwiki endnotexml epub fb2 gfm haddock html ipynb jats jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict mdoc mediawiki muse native odt opml org pod pptx ris rst rtf t2t textile tikiwiki tsv twiki typst vimwiki xlsx xml)'
    '--from[Reader format]:FORMAT:(asciidoc biblatex bibtex bits commonmark commonmark_x creole csljson csv djot docbook docx dokuwiki endnotexml epub fb2 gfm haddock html ipynb jats jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict mdoc mediawiki muse native odt opml org pod pptx ris rst rtf t2t textile tikiwiki tsv twiki typst vimwiki xlsx xml)'
    '--read[Reader format]:FORMAT:(asciidoc biblatex bibtex bits commonmark commonmark_x creole csljson csv djot docbook docx dokuwiki endnotexml epub fb2 gfm haddock html ipynb jats jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict mdoc mediawiki muse native odt opml org pod pptx ris rst rtf t2t textile tikiwiki tsv twiki typst vimwiki xlsx xml)'
    '-t[Writer format]:FORMAT:(ansi asciidoc asciidoc_legacy asciidoctor bbcode bbcode_fluxbb bbcode_hubzilla bbcode_phpbb bbcode_steam bbcode_xenforo beamer biblatex bibtex chunkedhtml commonmark commonmark_x context csljson djot docbook docbook4 docbook5 docx dokuwiki dzslides epub epub2 epub3 fb2 gfm haddock html html4 html5 icml ipynb jats jats_archiving jats_articleauthoring jats_publishing jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict markua mediawiki ms muse native odt opendocument opml org pdf plain pptx revealjs rst rtf s5 slideous slidy t2t tei texinfo textile typst vimdoc xml xwiki zimwiki)'
    '-w[Writer format]:FORMAT:(ansi asciidoc asciidoc_legacy asciidoctor bbcode bbcode_fluxbb bbcode_hubzilla bbcode_phpbb bbcode_steam bbcode_xenforo beamer biblatex bibtex chunkedhtml commonmark commonmark_x context csljson djot docbook docbook4 docbook5 docx dokuwiki dzslides epub epub2 epub3 fb2 gfm haddock html html4 html5 icml ipynb jats jats_archiving jats_articleauthoring jats_publishing jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict markua mediawiki ms muse native odt opendocument opml org pdf plain pptx revealjs rst rtf s5 slideous slidy t2t tei texinfo textile typst vimdoc xml xwiki zimwiki)'
    '--to[Writer format]:FORMAT:(ansi asciidoc asciidoc_legacy asciidoctor bbcode bbcode_fluxbb bbcode_hubzilla bbcode_phpbb bbcode_steam bbcode_xenforo beamer biblatex bibtex chunkedhtml commonmark commonmark_x context csljson djot docbook docbook4 docbook5 docx dokuwiki dzslides epub epub2 epub3 fb2 gfm haddock html html4 html5 icml ipynb jats jats_archiving jats_articleauthoring jats_publishing jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict markua mediawiki ms muse native odt opendocument opml org pdf plain pptx revealjs rst rtf s5 slideous slidy t2t tei texinfo textile typst vimdoc xml xwiki zimwiki)'
    '--write[Writer format]:FORMAT:(ansi asciidoc asciidoc_legacy asciidoctor bbcode bbcode_fluxbb bbcode_hubzilla bbcode_phpbb bbcode_steam bbcode_xenforo beamer biblatex bibtex chunkedhtml commonmark commonmark_x context csljson djot docbook docbook4 docbook5 docx dokuwiki dzslides epub epub2 epub3 fb2 gfm haddock html html4 html5 icml ipynb jats jats_archiving jats_articleauthoring jats_publishing jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict markua mediawiki ms muse native odt opendocument opml org pdf plain pptx revealjs rst rtf s5 slideous slidy t2t tei texinfo textile typst vimdoc xml xwiki zimwiki)'
    '-o[Output file]:FILE:_files'
    '--output[Output file]:FILE:_files'
    '--data-dir[Directory for data files]:DIRECTORY:_files'
    '-M[Metadata field KEY=VALUE]:KEY[=VALUE]:_files'
    '--metadata[Metadata field KEY=VALUE]:KEY[=VALUE]:_files'
    '--metadata-file[Metadata file]:FILE:_files'
    '-d[Defaults file]:FILE:_files'
    '--defaults[Defaults file]:FILE:_files'
    '--file-scope[Parse files before combining]'
    '--sandbox[Run pandoc in a sandbox]'
    '-s[Include header and footer]'
    '--standalone[Include header and footer]'
    '--template[Custom template file]:FILE:_files'
    '-V[Template variable KEY=VALUE]:KEY[=VALUE]:_files'
    '--variable[Template variable KEY=VALUE]:KEY[=VALUE]:_files'
    '--variable-json[Template variable KEY=JSON]:KEY[:JSON]:_files'
    '--wrap[Text wrapping mode]:auto|none|preserve:(auto none preserve)'
    '--ascii[Prefer ASCII output]'
    '--toc[Include table of contents]'
    '--table-of-contents[Include table of contents]'
    '--toc-depth[Number of TOC levels]:NUMBER:_files'
    '--lof[Include list of figures]'
    '--list-of-figures[Include list of figures]'
    '--lot[Include list of tables]'
    '--list-of-tables[Include list of tables]'
    '-N[Number section headings]'
    '--number-sections[Number section headings]'
    '--number-offset[Starting number for sections]:NUMBERS:_files'
    '--top-level-division[Top-level document division]:section|chapter|part:(section chapter part)'
    '--extract-media[Directory to extract media into]:PATH:_files'
    '--resource-path[Search path for resources]:SEARCHPATH:_files'
    '-H[File to include in the header]:FILE:_files'
    '--include-in-header[File to include in the header]:FILE:_files'
    '-B[File to include before the body]:FILE:_files'
    '--include-before-body[File to include before the body]:FILE:_files'
    '-A[File to include after the body]:FILE:_files'
    '--include-after-body[File to include after the body]:FILE:_files'
    '--no-highlight[Disable syntax highlighting]'
    '--highlight-style[Highlighting style]:STYLE:(pygments tango espresso zenburn kate monochrome breezedark haddock)'
    '--syntax-definition[Syntax definition XML file]:FILE:_files'
    '--syntax-highlighting[Syntax highlighting method]:none|default|idiomatic|<stylename>|<themepath>:(none default idiomatic)'
    '--dpi[DPI for imported images]:NUMBER:_files'
    '--eol[End-of-line characters]:crlf|lf|native:(crlf lf native)'
    '--columns[Line length in characters]:NUMBER:_files'
    '-p[Preserve tabs]'
    '--preserve-tabs[Preserve tabs]'
    '--tab-stop[Tab stop width]:NUMBER:_files'
    '--pdf-engine[Program used to produce PDF]:PROGRAM:(weasyprint wkhtmltopdf pagedjs-cli prince pdflatex lualatex xelatex latexmk tectonic pdflatex-dev lualatex-dev groff pdfroff typst context)'
    '--pdf-engine-opt[Flag to pass to the PDF engine]:STRING:_files'
    '--reference-doc[Custom reference doc]:FILE:_files'
    '--self-contained[Embed resources (deprecated)]'
    '--embed-resources[Embed referenced resources]'
    '--link-images[Link images in ODT rather than embedding]'
    '--request-header[HTTP header NAME=VALUE]:NAME=VALUE:_files'
    '--no-check-certificate[Disable certificate validation]'
    '--abbreviations[File with abbreviations]:FILE:_files'
    '--typst-input[Typst variable KEY=VALUE]:KEY=VALUE:_files'
    '--indented-code-classes[Classes for indented code blocks]:STRING:_files'
    '--default-image-extension[Default extension for images]:extension:_files'
    '-F[External JSON filter]:PROGRAM:_files'
    '--filter[External JSON filter]:PROGRAM:_files'
    '-L[Lua filter script]:SCRIPTPATH:_files'
    '--lua-filter[Lua filter script]:SCRIPTPATH:_files'
    '--shift-heading-level-by[Shift heading level by N]:NUMBER:_files'
    '--base-header-level[Base header level (deprecated)]:NUMBER:_files'
    '--track-changes[Handling of Word track-changes]:accept|reject|all:(accept reject all)'
    '--strip-comments[Strip HTML comments]'
    '--reference-links[Use reference links in HTML]'
    '--reference-location[Location of references]:block|section|document:(block section document)'
    '--figure-caption-position[Figure caption position]:above|below:(above below)'
    '--table-caption-position[Table caption position]:above|below:(above below)'
    '--markdown-headings[Markdown heading style]:setext|atx:(setext atx)'
    '--list-tables[Use list tables for RST]'
    '--listings[Use listings package (deprecated)]'
    '-i[Make list items display incrementally]'
    '--incremental[Make list items display incrementally]'
    '--slide-level[Header level used for slides]:NUMBER:_files'
    '--section-divs[Wrap sections in div tags]'
    '--html-q-tags[Use q tags for quotes in HTML]'
    '--email-obfuscation[Email obfuscation method]:none|javascript|references:(references javascript none)'
    '--id-prefix[Prefix for auto identifiers]:STRING:_files'
    '-T[Window title prefix]:STRING:_files'
    '--title-prefix[Window title prefix]:STRING:_files'
    '-c[CSS style sheet]:URL:_files'
    '--css[CSS style sheet]:URL:_files'
    '--epub-subdirectory[EPUB content subdirectory]:DIRNAME:_files'
    '--epub-cover-image[EPUB cover image]:FILE:_files'
    '--epub-title-page[URL or file for EPUB title page]:true|false:_files'
    '--epub-metadata[EPUB metadata file]:FILE:_files'
    '--epub-embed-font[Font file to embed in EPUB]:FILE:_files'
    '--split-level[Split level for chunked HTML or EPUB]:NUMBER:_files'
    '--chunk-template[Template for chunked HTML paths]:PATHTEMPLATE:_files'
    '--epub-chapter-level[Split level (deprecated)]:NUMBER:_files'
    '--ipynb-output[Handling of ipynb output cells]:all|none|best:(all none best)'
    '-C[Process citations]'
    '--citeproc[Process citations]'
    '--bibliography[Bibliography file]:FILE:_files'
    '--csl[CSL style file]:FILE:_files'
    '--citation-abbreviations[Citation abbreviations file]:FILE:_files'
    '--natbib[Use natbib citations in LaTeX]'
    '--biblatex[Use biblatex citations in LaTeX]'
    '--math-method[Specify method for rendering math in HTML]:METHOD:(plain mathml webtex mathjax katex gladtex)'
    '--mathml[Use MathML for HTML math]'
    '--webtex[Use WebTeX for HTML math]'
    '--mathjax[Use MathJax for HTML math]'
    '--katex[Use KaTeX for HTML math]'
    '--gladtex[Use gladTeX for HTML math]'
    '--trace[Turn on diagnostic tracing]'
    '--dump-args[Print output filename and arguments]'
    '--ignore-args[Ignore command-line arguments]'
    '--verbose[Verbose diagnostic output]'
    '--quiet[Suppress warning messages]'
    '--fail-if-warnings[Exit with error status if there were warnings]'
    '--log[Log messages in JSON format to this file]:FILE:_files'
    '--completion[Shell for which to print the completion script]'
    '--bash-completion[Print bash completion script (deprecated)]'
    '--list-input-formats[List supported input formats]'
    '--list-output-formats[List supported output formats]'
    '--list-extensions[List supported extensions]'
    '--list-highlight-languages[List highlighting languages]'
    '--list-highlight-styles[List highlighting styles]'
    '-D[Format to print template for]:FORMAT:(ansi asciidoc asciidoc_legacy asciidoctor bbcode bbcode_fluxbb bbcode_hubzilla bbcode_phpbb bbcode_steam bbcode_xenforo beamer biblatex bibtex chunkedhtml commonmark commonmark_x context csljson djot docbook docbook4 docbook5 docx dokuwiki dzslides epub epub2 epub3 fb2 gfm haddock html html4 html5 icml ipynb jats jats_archiving jats_articleauthoring jats_publishing jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict markua mediawiki ms muse native odt opendocument opml org pdf plain pptx revealjs rst rtf s5 slideous slidy t2t tei texinfo textile typst vimdoc xml xwiki zimwiki)'
    '--print-default-template[Format to print template for]:FORMAT:(ansi asciidoc asciidoc_legacy asciidoctor bbcode bbcode_fluxbb bbcode_hubzilla bbcode_phpbb bbcode_steam bbcode_xenforo beamer biblatex bibtex chunkedhtml commonmark commonmark_x context csljson djot docbook docbook4 docbook5 docx dokuwiki dzslides epub epub2 epub3 fb2 gfm haddock html html4 html5 icml ipynb jats jats_archiving jats_articleauthoring jats_publishing jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict markua mediawiki ms muse native odt opendocument opml org pdf plain pptx revealjs rst rtf s5 slideous slidy t2t tei texinfo textile typst vimdoc xml xwiki zimwiki)'
    '--print-default-data-file[Data file to print]:FILE:(MANUAL.txt abbreviations creole.lua default.csl docbook-entities.txt docx/[Content_Types].xml docx/_rels/.rels docx/docProps/app.xml docx/docProps/core.xml docx/docProps/custom.xml docx/word/_rels/document.xml.rels docx/word/_rels/footnotes.xml.rels docx/word/comments.xml docx/word/document.xml docx/word/fontTable.xml docx/word/footnotes.xml docx/word/numbering.xml docx/word/settings.xml docx/word/styles.xml docx/word/theme/theme1.xml docx/word/webSettings.xml dzslides/template.html epub.css init.lua odt/META-INF/manifest.xml odt/content.xml odt/manifest.rdf odt/meta.xml odt/mimetype odt/styles.xml pptx/[Content_Types].xml pptx/_rels/.rels pptx/docProps/app.xml pptx/docProps/core.xml pptx/ppt/_rels/presentation.xml.rels pptx/ppt/notesMasters/_rels/notesMaster1.xml.rels pptx/ppt/notesMasters/notesMaster1.xml pptx/ppt/notesSlides/_rels/notesSlide1.xml.rels pptx/ppt/notesSlides/_rels/notesSlide2.xml.rels pptx/ppt/notesSlides/notesSlide1.xml pptx/ppt/notesSlides/notesSlide2.xml pptx/ppt/presProps.xml pptx/ppt/presentation.xml pptx/ppt/slideLayouts/_rels/slideLayout1.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout10.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout11.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout2.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout3.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout4.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout5.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout6.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout7.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout8.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout9.xml.rels pptx/ppt/slideLayouts/slideLayout1.xml pptx/ppt/slideLayouts/slideLayout10.xml pptx/ppt/slideLayouts/slideLayout11.xml pptx/ppt/slideLayouts/slideLayout2.xml pptx/ppt/slideLayouts/slideLayout3.xml pptx/ppt/slideLayouts/slideLayout4.xml pptx/ppt/slideLayouts/slideLayout5.xml pptx/ppt/slideLayouts/slideLayout6.xml pptx/ppt/slideLayouts/slideLayout7.xml pptx/ppt/slideLayouts/slideLayout8.xml pptx/ppt/slideLayouts/slideLayout9.xml pptx/ppt/slideMasters/_rels/slideMaster1.xml.rels pptx/ppt/slideMasters/slideMaster1.xml pptx/ppt/slides/_rels/slide1.xml.rels pptx/ppt/slides/_rels/slide2.xml.rels pptx/ppt/slides/_rels/slide3.xml.rels pptx/ppt/slides/_rels/slide4.xml.rels pptx/ppt/slides/slide1.xml pptx/ppt/slides/slide2.xml pptx/ppt/slides/slide3.xml pptx/ppt/slides/slide4.xml pptx/ppt/tableStyles.xml pptx/ppt/theme/theme1.xml pptx/ppt/theme/theme2.xml pptx/ppt/viewProps.xml reference.docx reference.odt reference.pptx templates/affiliations.jats templates/after-header-includes.latex templates/article.jats_publishing templates/common.latex templates/default.ansi templates/default.asciidoc templates/default.bbcode templates/default.beamer templates/default.biblatex templates/default.bibtex templates/default.chunkedhtml templates/default.commonmark templates/default.context templates/default.djot templates/default.docbook4 templates/default.docbook5 templates/default.dokuwiki templates/default.dzslides templates/default.epub2 templates/default.epub3 templates/default.haddock templates/default.html4 templates/default.html5 templates/default.icml templates/default.jats_archiving templates/default.jats_articleauthoring templates/default.jats_publishing templates/default.jira templates/default.latex templates/default.man templates/default.markdown templates/default.markua templates/default.mediawiki templates/default.ms templates/default.muse templates/default.opendocument templates/default.openxml templates/default.opml templates/default.org templates/default.plain templates/default.revealjs templates/default.rst templates/default.rtf templates/default.s5 templates/default.slideous templates/default.slidy templates/default.t2t templates/default.tei templates/default.texinfo templates/default.textile templates/default.typst templates/default.vimdoc templates/default.xwiki templates/default.zimwiki templates/document-metadata.latex templates/font-settings.latex templates/fonts.latex templates/hypersetup.latex templates/passoptions.latex templates/styles.citations.html templates/styles.html templates/template.typst translations/af.yaml translations/alt.yaml translations/am.yaml translations/ar.yaml translations/as.yaml translations/ast.yaml translations/az.yaml translations/be.yaml translations/bg.yaml translations/bn.yaml translations/bo.yaml translations/br.yaml translations/bs.yaml translations/bua.yaml translations/ca.yaml translations/ckb-Arab.yaml translations/ckb-Latn.yaml translations/cs.yaml translations/cu.yaml translations/cy.yaml translations/cz.yaml translations/da.yaml translations/de.yaml translations/dsb.yaml translations/el.yaml translations/en.yaml translations/eo.yaml translations/es-ES.yaml translations/es-MX.yaml translations/es.yaml translations/et.yaml translations/eu.yaml translations/fa.yaml translations/fi.yaml translations/fil.yaml translations/fr.yaml translations/fur.yaml translations/ga.yaml translations/gd.yaml translations/gl.yaml translations/grc.yaml translations/gu.yaml translations/ha.yaml translations/he.yaml translations/hi.yaml translations/hr.yaml translations/hsb.yaml translations/hu.yaml translations/hy.yaml translations/ia.yaml translations/id.yaml translations/is.yaml translations/it.yaml translations/ja.yaml translations/ka.yaml translations/km.yaml translations/kmr-Arab.yaml translations/kmr-Latn.yaml translations/kn.yaml translations/ko.yaml translations/la.yaml translations/lb.yaml translations/lo.yaml translations/lt.yaml translations/lv.yaml translations/mk.yaml translations/ml.yaml translations/mn.yaml translations/mr.yaml translations/ms.yaml translations/nb.yaml translations/nko.yaml translations/nl.yaml translations/nn.yaml translations/no.yaml translations/oc.yaml translations/or.yaml translations/pa.yaml translations/pl.yaml translations/pms.yaml translations/pt-BR.yaml translations/pt-PT.yaml translations/pt.yaml translations/rm.yaml translations/ro.yaml translations/ru.yaml translations/se.yaml translations/si.yaml translations/sk.yaml translations/sl.yaml translations/sq.yaml translations/sr-Cyrl.yaml translations/sr-Latn.yaml translations/sr.yaml translations/sv.yaml translations/ta.yaml translations/te.yaml translations/th.yaml translations/tk.yaml translations/tr.yaml translations/ua.yaml translations/ug.yaml translations/uk.yaml translations/ur.yaml translations/vi.yaml translations/zh-Hans.yaml translations/zh-Hant.yaml)'
    '--print-highlight-style[Highlighting style]:STYLE:(pygments tango espresso zenburn kate monochrome breezedark haddock)'
    '-v[Print version]'
    '--version[Print version]'
    '-h[Show help]'
    '--help[Show help]'
    '*:files:_files'
  )
  _arguments -s -S $args
}

_pandoc "$@"

.
```

```
% pandoc --completion=fish
^D
complete -c pandoc -l from -d "Reader format" -r -a "asciidoc biblatex bibtex bits commonmark commonmark_x creole csljson csv djot docbook docx dokuwiki endnotexml epub fb2 gfm haddock html ipynb jats jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict mdoc mediawiki muse native odt opml org pod pptx ris rst rtf t2t textile tikiwiki tsv twiki typst vimwiki xlsx xml"
complete -c pandoc -l to -d "Writer format" -r -a "ansi asciidoc asciidoc_legacy asciidoctor bbcode bbcode_fluxbb bbcode_hubzilla bbcode_phpbb bbcode_steam bbcode_xenforo beamer biblatex bibtex chunkedhtml commonmark commonmark_x context csljson djot docbook docbook4 docbook5 docx dokuwiki dzslides epub epub2 epub3 fb2 gfm haddock html html4 html5 icml ipynb jats jats_archiving jats_articleauthoring jats_publishing jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict markua mediawiki ms muse native odt opendocument opml org pdf plain pptx revealjs rst rtf s5 slideous slidy t2t tei texinfo textile typst vimdoc xml xwiki zimwiki"
complete -c pandoc -s o -l output -d "Output file" -r
complete -c pandoc -l data-dir -d "Directory for data files" -r
complete -c pandoc -s M -l metadata -d "Metadata field KEY=VALUE" -r
complete -c pandoc -l metadata-file -d "Metadata file" -r
complete -c pandoc -s d -l defaults -d "Defaults file" -r
complete -c pandoc -l file-scope -d "Parse files before combining"
complete -c pandoc -l sandbox -d "Run pandoc in a sandbox"
complete -c pandoc -s s -l standalone -d "Include header and footer"
complete -c pandoc -l template -d "Custom template file" -r
complete -c pandoc -s V -l variable -d "Template variable KEY=VALUE" -r
complete -c pandoc -l variable-json -d "Template variable KEY=JSON" -r
complete -c pandoc -l wrap -d "Text wrapping mode" -r -a "auto none preserve"
complete -c pandoc -l ascii -d "Prefer ASCII output"
complete -c pandoc -l toc -d "Include table of contents"
complete -c pandoc -l toc-depth -d "Number of TOC levels" -r
complete -c pandoc -l lof -d "Include list of figures"
complete -c pandoc -l lot -d "Include list of tables"
complete -c pandoc -s N -l number-sections -d "Number section headings"
complete -c pandoc -l number-offset -d "Starting number for sections" -r
complete -c pandoc -l top-level-division -d "Top-level document division" -r -a "section chapter part"
complete -c pandoc -l extract-media -d "Directory to extract media into" -r
complete -c pandoc -l resource-path -d "Search path for resources" -r
complete -c pandoc -s H -l include-in-header -d "File to include in the header" -r
complete -c pandoc -s B -l include-before-body -d "File to include before the body" -r
complete -c pandoc -s A -l include-after-body -d "File to include after the body" -r
complete -c pandoc -l no-highlight -d "Disable syntax highlighting"
complete -c pandoc -l highlight-style -d "Highlighting style" -r -a "pygments tango espresso zenburn kate monochrome breezedark haddock"
complete -c pandoc -l syntax-definition -d "Syntax definition XML file" -r
complete -c pandoc -l syntax-highlighting -d "Syntax highlighting method" -r -a "none default idiomatic"
complete -c pandoc -l dpi -d "DPI for imported images" -r
complete -c pandoc -l eol -d "End-of-line characters" -r -a "crlf lf native"
complete -c pandoc -l columns -d "Line length in characters" -r
complete -c pandoc -s p -l preserve-tabs -d "Preserve tabs"
complete -c pandoc -l tab-stop -d "Tab stop width" -r
complete -c pandoc -l pdf-engine -d "Program used to produce PDF" -r -a "weasyprint wkhtmltopdf pagedjs-cli prince pdflatex lualatex xelatex latexmk tectonic pdflatex-dev lualatex-dev groff pdfroff typst context"
complete -c pandoc -l pdf-engine-opt -d "Flag to pass to the PDF engine" -r
complete -c pandoc -l reference-doc -d "Custom reference doc" -r
complete -c pandoc -l self-contained -d "Embed resources (deprecated)"
complete -c pandoc -l embed-resources -d "Embed referenced resources"
complete -c pandoc -l link-images -d "Link images in ODT rather than embedding"
complete -c pandoc -l request-header -d "HTTP header NAME=VALUE" -r
complete -c pandoc -l no-check-certificate -d "Disable certificate validation"
complete -c pandoc -l abbreviations -d "File with abbreviations" -r
complete -c pandoc -l typst-input -d "Typst variable KEY=VALUE" -r
complete -c pandoc -l indented-code-classes -d "Classes for indented code blocks" -r
complete -c pandoc -l default-image-extension -d "Default extension for images" -r
complete -c pandoc -s F -l filter -d "External JSON filter" -r
complete -c pandoc -s L -l lua-filter -d "Lua filter script" -r
complete -c pandoc -l shift-heading-level-by -d "Shift heading level by N" -r
complete -c pandoc -l base-header-level -d "Base header level (deprecated)" -r
complete -c pandoc -l track-changes -d "Handling of Word track-changes" -r -a "accept reject all"
complete -c pandoc -l strip-comments -d "Strip HTML comments"
complete -c pandoc -l reference-links -d "Use reference links in HTML"
complete -c pandoc -l reference-location -d "Location of references" -r -a "block section document"
complete -c pandoc -l figure-caption-position -d "Figure caption position" -r -a "above below"
complete -c pandoc -l table-caption-position -d "Table caption position" -r -a "above below"
complete -c pandoc -l markdown-headings -d "Markdown heading style" -r -a "setext atx"
complete -c pandoc -l list-tables -d "Use list tables for RST"
complete -c pandoc -l listings -d "Use listings package (deprecated)"
complete -c pandoc -s i -l incremental -d "Make list items display incrementally"
complete -c pandoc -l slide-level -d "Header level used for slides" -r
complete -c pandoc -l section-divs -d "Wrap sections in div tags"
complete -c pandoc -l html-q-tags -d "Use q tags for quotes in HTML"
complete -c pandoc -l email-obfuscation -d "Email obfuscation method" -r -a "references javascript none"
complete -c pandoc -l id-prefix -d "Prefix for auto identifiers" -r
complete -c pandoc -s T -l title-prefix -d "Window title prefix" -r
complete -c pandoc -s c -l css -d "CSS style sheet" -r
complete -c pandoc -l epub-subdirectory -d "EPUB content subdirectory" -r
complete -c pandoc -l epub-cover-image -d "EPUB cover image" -r
complete -c pandoc -l epub-title-page -d "URL or file for EPUB title page" -r
complete -c pandoc -l epub-metadata -d "EPUB metadata file" -r
complete -c pandoc -l epub-embed-font -d "Font file to embed in EPUB" -r
complete -c pandoc -l split-level -d "Split level for chunked HTML or EPUB" -r
complete -c pandoc -l chunk-template -d "Template for chunked HTML paths" -r
complete -c pandoc -l epub-chapter-level -d "Split level (deprecated)" -r
complete -c pandoc -l ipynb-output -d "Handling of ipynb output cells" -r -a "all none best"
complete -c pandoc -s C -l citeproc -d "Process citations"
complete -c pandoc -l bibliography -d "Bibliography file" -r
complete -c pandoc -l csl -d "CSL style file" -r
complete -c pandoc -l citation-abbreviations -d "Citation abbreviations file" -r
complete -c pandoc -l natbib -d "Use natbib citations in LaTeX"
complete -c pandoc -l biblatex -d "Use biblatex citations in LaTeX"
complete -c pandoc -l math-method -d "Specify method for rendering math in HTML" -r -a "plain mathml webtex mathjax katex gladtex"
complete -c pandoc -l mathml -d "Use MathML for HTML math"
complete -c pandoc -l webtex -d "Use WebTeX for HTML math"
complete -c pandoc -l mathjax -d "Use MathJax for HTML math"
complete -c pandoc -l katex -d "Use KaTeX for HTML math"
complete -c pandoc -l gladtex -d "Use gladTeX for HTML math"
complete -c pandoc -l trace -d "Turn on diagnostic tracing"
complete -c pandoc -l dump-args -d "Print output filename and arguments"
complete -c pandoc -l ignore-args -d "Ignore command-line arguments"
complete -c pandoc -l verbose -d "Verbose diagnostic output"
complete -c pandoc -l quiet -d "Suppress warning messages"
complete -c pandoc -l fail-if-warnings -d "Exit with error status if there were warnings"
complete -c pandoc -l log -d "Log messages in JSON format to this file" -r
complete -c pandoc -l completion -d "Shell for which to print the completion script"
complete -c pandoc -l bash-completion -d "Print bash completion script (deprecated)"
complete -c pandoc -l list-input-formats -d "List supported input formats"
complete -c pandoc -l list-output-formats -d "List supported output formats"
complete -c pandoc -l list-extensions -d "List supported extensions"
complete -c pandoc -l list-highlight-languages -d "List highlighting languages"
complete -c pandoc -l list-highlight-styles -d "List highlighting styles"
complete -c pandoc -s D -l print-default-template -d "Format to print template for" -r -a "ansi asciidoc asciidoc_legacy asciidoctor bbcode bbcode_fluxbb bbcode_hubzilla bbcode_phpbb bbcode_steam bbcode_xenforo beamer biblatex bibtex chunkedhtml commonmark commonmark_x context csljson djot docbook docbook4 docbook5 docx dokuwiki dzslides epub epub2 epub3 fb2 gfm haddock html html4 html5 icml ipynb jats jats_archiving jats_articleauthoring jats_publishing jira json latex man markdown markdown_github markdown_mmd markdown_phpextra markdown_strict markua mediawiki ms muse native odt opendocument opml org pdf plain pptx revealjs rst rtf s5 slideous slidy t2t tei texinfo textile typst vimdoc xml xwiki zimwiki"
complete -c pandoc -l print-default-data-file -d "Data file to print" -r -a "MANUAL.txt abbreviations creole.lua default.csl docbook-entities.txt docx/[Content_Types].xml docx/_rels/.rels docx/docProps/app.xml docx/docProps/core.xml docx/docProps/custom.xml docx/word/_rels/document.xml.rels docx/word/_rels/footnotes.xml.rels docx/word/comments.xml docx/word/document.xml docx/word/fontTable.xml docx/word/footnotes.xml docx/word/numbering.xml docx/word/settings.xml docx/word/styles.xml docx/word/theme/theme1.xml docx/word/webSettings.xml dzslides/template.html epub.css init.lua odt/META-INF/manifest.xml odt/content.xml odt/manifest.rdf odt/meta.xml odt/mimetype odt/styles.xml pptx/[Content_Types].xml pptx/_rels/.rels pptx/docProps/app.xml pptx/docProps/core.xml pptx/ppt/_rels/presentation.xml.rels pptx/ppt/notesMasters/_rels/notesMaster1.xml.rels pptx/ppt/notesMasters/notesMaster1.xml pptx/ppt/notesSlides/_rels/notesSlide1.xml.rels pptx/ppt/notesSlides/_rels/notesSlide2.xml.rels pptx/ppt/notesSlides/notesSlide1.xml pptx/ppt/notesSlides/notesSlide2.xml pptx/ppt/presProps.xml pptx/ppt/presentation.xml pptx/ppt/slideLayouts/_rels/slideLayout1.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout10.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout11.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout2.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout3.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout4.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout5.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout6.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout7.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout8.xml.rels pptx/ppt/slideLayouts/_rels/slideLayout9.xml.rels pptx/ppt/slideLayouts/slideLayout1.xml pptx/ppt/slideLayouts/slideLayout10.xml pptx/ppt/slideLayouts/slideLayout11.xml pptx/ppt/slideLayouts/slideLayout2.xml pptx/ppt/slideLayouts/slideLayout3.xml pptx/ppt/slideLayouts/slideLayout4.xml pptx/ppt/slideLayouts/slideLayout5.xml pptx/ppt/slideLayouts/slideLayout6.xml pptx/ppt/slideLayouts/slideLayout7.xml pptx/ppt/slideLayouts/slideLayout8.xml pptx/ppt/slideLayouts/slideLayout9.xml pptx/ppt/slideMasters/_rels/slideMaster1.xml.rels pptx/ppt/slideMasters/slideMaster1.xml pptx/ppt/slides/_rels/slide1.xml.rels pptx/ppt/slides/_rels/slide2.xml.rels pptx/ppt/slides/_rels/slide3.xml.rels pptx/ppt/slides/_rels/slide4.xml.rels pptx/ppt/slides/slide1.xml pptx/ppt/slides/slide2.xml pptx/ppt/slides/slide3.xml pptx/ppt/slides/slide4.xml pptx/ppt/tableStyles.xml pptx/ppt/theme/theme1.xml pptx/ppt/theme/theme2.xml pptx/ppt/viewProps.xml reference.docx reference.odt reference.pptx templates/affiliations.jats templates/after-header-includes.latex templates/article.jats_publishing templates/common.latex templates/default.ansi templates/default.asciidoc templates/default.bbcode templates/default.beamer templates/default.biblatex templates/default.bibtex templates/default.chunkedhtml templates/default.commonmark templates/default.context templates/default.djot templates/default.docbook4 templates/default.docbook5 templates/default.dokuwiki templates/default.dzslides templates/default.epub2 templates/default.epub3 templates/default.haddock templates/default.html4 templates/default.html5 templates/default.icml templates/default.jats_archiving templates/default.jats_articleauthoring templates/default.jats_publishing templates/default.jira templates/default.latex templates/default.man templates/default.markdown templates/default.markua templates/default.mediawiki templates/default.ms templates/default.muse templates/default.opendocument templates/default.openxml templates/default.opml templates/default.org templates/default.plain templates/default.revealjs templates/default.rst templates/default.rtf templates/default.s5 templates/default.slideous templates/default.slidy templates/default.t2t templates/default.tei templates/default.texinfo templates/default.textile templates/default.typst templates/default.vimdoc templates/default.xwiki templates/default.zimwiki templates/document-metadata.latex templates/font-settings.latex templates/fonts.latex templates/hypersetup.latex templates/passoptions.latex templates/styles.citations.html templates/styles.html templates/template.typst translations/af.yaml translations/alt.yaml translations/am.yaml translations/ar.yaml translations/as.yaml translations/ast.yaml translations/az.yaml translations/be.yaml translations/bg.yaml translations/bn.yaml translations/bo.yaml translations/br.yaml translations/bs.yaml translations/bua.yaml translations/ca.yaml translations/ckb-Arab.yaml translations/ckb-Latn.yaml translations/cs.yaml translations/cu.yaml translations/cy.yaml translations/cz.yaml translations/da.yaml translations/de.yaml translations/dsb.yaml translations/el.yaml translations/en.yaml translations/eo.yaml translations/es-ES.yaml translations/es-MX.yaml translations/es.yaml translations/et.yaml translations/eu.yaml translations/fa.yaml translations/fi.yaml translations/fil.yaml translations/fr.yaml translations/fur.yaml translations/ga.yaml translations/gd.yaml translations/gl.yaml translations/grc.yaml translations/gu.yaml translations/ha.yaml translations/he.yaml translations/hi.yaml translations/hr.yaml translations/hsb.yaml translations/hu.yaml translations/hy.yaml translations/ia.yaml translations/id.yaml translations/is.yaml translations/it.yaml translations/ja.yaml translations/ka.yaml translations/km.yaml translations/kmr-Arab.yaml translations/kmr-Latn.yaml translations/kn.yaml translations/ko.yaml translations/la.yaml translations/lb.yaml translations/lo.yaml translations/lt.yaml translations/lv.yaml translations/mk.yaml translations/ml.yaml translations/mn.yaml translations/mr.yaml translations/ms.yaml translations/nb.yaml translations/nko.yaml translations/nl.yaml translations/nn.yaml translations/no.yaml translations/oc.yaml translations/or.yaml translations/pa.yaml translations/pl.yaml translations/pms.yaml translations/pt-BR.yaml translations/pt-PT.yaml translations/pt.yaml translations/rm.yaml translations/ro.yaml translations/ru.yaml translations/se.yaml translations/si.yaml translations/sk.yaml translations/sl.yaml translations/sq.yaml translations/sr-Cyrl.yaml translations/sr-Latn.yaml translations/sr.yaml translations/sv.yaml translations/ta.yaml translations/te.yaml translations/th.yaml translations/tk.yaml translations/tr.yaml translations/ua.yaml translations/ug.yaml translations/uk.yaml translations/ur.yaml translations/vi.yaml translations/zh-Hans.yaml translations/zh-Hant.yaml"
complete -c pandoc -l print-highlight-style -d "Highlighting style" -r -a "pygments tango espresso zenburn kate monochrome breezedark haddock"
complete -c pandoc -s v -l version -d "Print version"
complete -c pandoc -s h -l help -d "Show help"

.
```
