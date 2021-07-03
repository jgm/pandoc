% FAQs

::: faqs

## How can I convert a whole directory of files from Markdown to RTF?

On linux or OSX:

    for f in *.txt; do pandoc "$f" -s -o "${f%.txt}.rtf"; done


In Windows Powershell:

    gci -r -i *.txt |foreach{$rtf=$_.directoryname+"\"+$_.basename+".rtf";pandoc -f markdown -s $_.name -o $rtf}

## I used pandoc to convert a document to ICML (or OPML or RTF), and when I try to open it I'm told it's invalid.  What have I done wrong?

Be sure to use the `-s` or `--standalone` flag, or you just get a
fragment, not a full document with the required header:

    pandoc -s -f markdown -t icml -o my.icml my.md

## I get a blank document when I try to convert a markdown document in Chinese to PDF.

By default, pandoc uses pdflatex to generate the PDF, and pdflatex
doesn't handle Chinese characters. But you can change the default to
use xelatex instead. You should also make sure you're using a font
with Chinese glyphs.  For example:

    pandoc -o c.pdf --pdf-engine=xelatex -V mainfont='Adobe Ming Std'

## The Windows installer does a single user install, rather than installing pandoc for all users. How can I install pandoc for all users?

Run the following command as admin:

    msiexec /i pandoc-VERSION.msi ALLUSERS=1

This will put pandoc in `C:\Program Files\Pandoc`.
You can install Pandoc to a different directory by setting APPLICATIONFOLDER parameter,
for example:

    msiexec /i pandoc-1.11.1.msi ALLUSERS=1 APPLICATIONFOLDER="C:\Pandoc"

## How do I change the margins in PDF output?

The option

    -V geometry:margin=1in

will set the margins to one inch on each side.  If you don't want uniform
margins, you can do something like

    -V geometry:"top=2cm, bottom=1.5cm, left=1cm, right=1cm"

Or

    -V geometry:"left=3cm, width=10cm"

For more options, see the documentation for the LaTeX [geometry
package](https://www.ctan.org/pkg/geometry).

## How does pandoc compare to multimarkdown?

Here is a [wiki
page](https://github.com/jgm/pandoc/wiki/Pandoc-vs-Multimarkdown)
comparing the two.

## When I specify an image width of 50% and convert to LaTeX, pandoc sets the height to textheight and the aspect ratio isn't preserved. How can I prevent this?

For example, if you convert an image with `{width="50%"}`, the LaTeX produced
will be `\includegraphics[width=0.5\textwidth,height=\textheight]`.

This output presupposes the following code in pandoc's default latex
template:

```
% Scale images if necessary, so that they will not overflow the page
% margins by default, and it is still possible to overwrite the defaults
% using explicit options in \includegraphics[width, height, ...]{}
\setkeys{Gin}{width=\maxwidth,height=\maxheight,keepaspectratio}
```

If you don't have this in your custom template, you should
add it.  If we didn't set the `height` explicitly in this way,
the image would not be resized correctly unless it was
being resized to smaller than its original size.

## Pandoc sometimes uses too much memory. How can I limit the memory used by pandoc?

`pandoc +RTS -M30m -RTS` will limit heap memory to 30MB.
When converting a document requires more than this, an out of
memory error will be issued.

## When using `--include-in-header` with PDF or LaTeX output, how do I reference tex declarations coming after `$header-includes$` in the default template?

For various reasons, the `$header-includes$` are not at the very
end of the LaTeX preamble.  This poses a problem when the code
you are inserting depends on declarations in the preamble coming
after the `$header-includes$` location. For example, you might
want to reference the `\author` and `\title` metadata values
(set at the very bottom of the preamble) and present them in
margins. In that case you can wrap your code in `etoolbox`'s
`\AtEndPreamble`.  The technique is demonstrated in [this
gist](https://gist.github.com/JohnLukeBentley/9dda6166b9ee5c4127afd2b8cd16b70a).
When using `\AtEndPreamble`, keep any `makeatletter` or
`makeatother` outside of the `\AtEndPreamble`, as shown in the
example.

## How can I convert PDFs to other formats using pandoc?

You can't. You can try opening the PDF in Word or Google Docs
and saving in a format from which pandoc can convert directly.

## Do I really need to install a 1 GB TeX installation to produce a PDF using pandoc?

No.  You can get by with a relatively small TeX installation,
for example, by starting with MacTeX's Basic TeX distribution
and using the `tlmgr` tool to install a few packages required by pandoc
(see https://pandoc.org/MANUAL.html#creating-a-pdf).

Or, you can produce PDFs via HTML and `wkhtmltopdf`,
or via groff ms and `pdfroff`.  (These don't produce as nice
topography as TeX, particularly when it comes to math, but they
may be fine for many purposes.)


:::

