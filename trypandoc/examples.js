const examples = {
  ["Hello world"]:
    { text: '*Hello* world!',
      from: 'markdown',
      to: 'html5'},
  ["BibTeX to CSL JSON"]:
    { text: `@BOOK{Wurm2011-ho,
  title     = "{Substanz und Qualität : Ein Beitrag zur Interpretation der
               plotinischen Traktate VI,1, 2 und 3}",
  author    = "Wurm, Klaus",
  publisher = "De Gruyter",
  series    = "Quellen und Studien zur Philosophie",
  edition   = "Reprint 2011",
  year      =  2011,
  address   = "Berlin",
  keywords  = "!!! Plotinus translation",
  language  = "de"
}`,
      from: 'bibtex',
      to: 'csljson' },
  ["Markdown to Docbook with citations"]:
  { text: `---
references:
- author:
  - family: Salam
    given: Abdus
  container-title: "Elementary particle theory: Relativistic groups and
    analyticity. Proceedings of the eighth Nobel symposium"
  editor:
  - family: Svartholm
    given: Nils
  event-date: 1968-05-19/1968-05-25
  event-place: Aspenäsgarden, Lerum
  id: salam
  issued: 1968
  page: 367-377
  publisher: Almquist & Wiksell
  publisher-place: Stockholm
  title: Weak and electromagnetic interactions
  type: paper-conference
---

@salam [p. 370] says some interesting things.`,
    from: 'markdown',
    to: 'docbook5',
    standalone: true,
    citeproc: true,
    files: {} },
  ["MediaWiki to docx with equations"]:
  { text: `Just as the components of a vector change when we change the [[basis (linear algebra)|basis]] of the vector space, the components of a tensor also change under such a transformation.  Each type of tensor comes equipped with a ''transformation law'' that details how the components of the tensor respond to a [[change of basis]].  The components of a vector can respond in two distinct ways to a [[change of basis]] (see [[covariance and contravariance of vectors]]), where the new [[basis vectors]] <math>\\mathbf{\\hat{e}}_i</math> are expressed in terms of the old basis vectors <math>\\mathbf{e}_j</math> as,
:<math>\\mathbf{\\hat{e}}_i = \\sum_{j=1}^n \\mathbf{e}_j R^j_i = \\mathbf{e}_j R^j_i .</math>

Here ''R''<sup>'' j''</sup><sub>''i''</sub> are the entries of the change of basis matrix, and in the rightmost expression the [[summation]] sign was suppressed: this is the [[Einstein summation convention]], which will be used throughout this article.<ref group="Note">The Einstein summation convention, in brief, requires the sum to be taken over all values of the index whenever the same symbol appears as a subscript and superscript in the same term.  For example, under this convention <math>B_i C^i = B_1 C^1 + B_2 C^2 + \\cdots B_n C^n</math></ref>  The components ''v''<sup>''i''</sup> of a column vector '''v''' transform with the [[matrix inverse|inverse]] of the matrix ''R'',
:<math>\\hat{v}^i = \\left(R^{-1}\\right)^i_j v^j,</math>

where the hat denotes the components in the new basis.  This is called a ''contravariant'' transformation law, because the vector components transform by the ''inverse'' of the change of basis.  In contrast, the components, ''w''<sub>''i''</sub>, of a covector (or row vector), '''w''', transform with the matrix ''R'' itself,
:<math>\\hat{w}_i = w_j R^j_i .</math>`,
    from: 'mediawiki',
    to: 'docx',
    standalone: true },

  ["Man page to ConTeXt"]:
  { text: `.TP
\\f[C]-L\\f[R] \\f[I]SCRIPT\\f[R], \\f[C]--lua-filter=\\f[R]\\f[I]SCRIPT\\f[R]
Transform the document in a similar fashion as JSON filters (see
\\f[C]--filter\\f[R]), but use pandoc\\[cq]s built-in Lua filtering system.
The given Lua script is expected to return a list of Lua filters which
will be applied in order.
Each Lua filter must contain element-transforming functions indexed by
the name of the AST element on which the filter function should be
applied.
.RS
.PP
The \\f[C]pandoc\\f[R] Lua module provides helper functions for element
creation.
It is always loaded into the script\\[cq]s Lua environment.
.PP
See the Lua filters documentation for further details.
.PP
In order of preference, pandoc will look for Lua filters in
.IP "1." 3
a specified full or relative path,
.IP "2." 3
\\f[C]$DATADIR/filters\\f[R] where \\f[C]$DATADIR\\f[R] is the user data
directory (see \\f[C]--data-dir\\f[R], above).
.PP
Filters, Lua filters, and citeproc processing are applied in the order
specified on the command line.
.RE
.TP
\\f[C]-M\\f[R] \\f[I]KEY\\f[R][\\f[C]=\\f[R]\\f[I]VAL\\f[R]], \\f[C]--metadata=\\f[R]\\f[I]KEY\\f[R][\\f[C]:\\f[R]\\f[I]VAL\\f[R]]
Set the metadata field \\f[I]KEY\\f[R] to the value \\f[I]VAL\\f[R].
A value specified on the command line overrides a value specified in the
document using YAML metadata blocks.
Values will be parsed as YAML boolean or string values.
If no value is specified, the value will be treated as Boolean true.
Like \\f[C]--variable\\f[R], \\f[C]--metadata\\f[R] causes template
variables to be set.
But unlike \\f[C]--variable\\f[R], \\f[C]--metadata\\f[R] affects the
metadata of the underlying document (which is accessible from filters
and may be printed in some output formats) and metadata values will be
escaped when inserted into the template.`,
    from: 'man',
    to: 'context' },
  ["LaTeX with macros to reStructuredText"]:
  { text: `% from https://en.wikibooks.org/wiki/LaTeX/Macros
\\newcommand{\\wbalTwo}[2][Wikimedia]{
This is the Wikibook about LaTeX
supported by {#1} and {#2}!}

\\begin{itemize}
\\item \\wbalTwo{John Doe}
\\item \\wbalTwo[lots of users]{John Doe}
\\end{itemize}`,
    from: 'latex',
    to: 'rst',
    standalone: true,
    citeproc: false,
    files: {} },

  ["CSV table to org"]:
  { text: `"Year", "Score", "Title"
1968,  86, "Greetings"
1970,  17, "Bloody Mama"
1970,  73, "Hi, Mom!"
1971,  40, "Born to Win"
1973,  98, "Mean Streets"
1973,  88, "Bang the Drum Slowly"
1974,  97, "The Godfather, Part II"
1976,  41, "The Last Tycoon"
1976,  99, "Taxi Driver"`,
    from: 'csv',
    to: 'org' },

  ["Markdown citations to plain with CSL style"]:
  { text: `---
csl: 'le-tapuscrit-note.csl'
lang: fr-FR
bibliography: refs.bib
---

Foo [@legras_michel_2010].`,
    from: 'markdown',
    to: 'plain',
    citeproc: true,
    files: {
      ["refs.bib"]:
        `@book{legras_michel_2010,
  author = {Le~Gras, Gwénaëlle},
  publisher = {Scope},
  title = {Michel Simon~: l’art de la disgrâce},
  series = {Jeux d’acteurs},
  date = {2010},
  address = {Paris},
  isbn = {978-2-912573-52-0},
  langid = {fre}}`,
      ["le-tapuscrit-note.csl"]:
        `<?xml version="1.0" encoding="utf-8"?>
<style xmlns="http://purl.org/net/xbiblio/csl" class="note" default-locale="fr-FR" version="1.0" page-range-format="expanded">
  <info>
    <title>Le tapuscrit (École des hautes études en sciences sociales) (note, French)</title>
    <title-short>Tapuscrit-EHESS</title-short>
    <id>http://www.zotero.org/styles/le-tapuscrit-note</id>
    <link href="http://www.zotero.org/styles/le-tapuscrit-note" rel="self"/>
    <link href="http://www.editions.ehess.fr/ouvrages/ouvrage/le-tapuscrit/" rel="documentation"/>
    <author>
      <name>Franziska Heimburger</name>
      <email>zotero@franziska.fr</email>
    </author>
    <category citation-format="note"/>
    <category field="social_science"/>
    <category field="generic-base"/>
    <updated>2018-07-12T11:20:37+00:00</updated>
    <rights license="http://creativecommons.org/licenses/by-sa/3.0/">This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 License</rights>
  </info>
  <locale xml:lang="fr">
    <terms>
      <term name="ordinal-01">ère</term>
      <term name="ordinal-02">e</term>
      <term name="ordinal-03">e</term>
      <term name="ordinal-04">e</term>
      <term name="cited">op.&#160;cit.</term>
      <term name="page" form="short">p.</term>
      <term name="editor" form="short">
        <single>ed.</single>
        <multiple>eds.</multiple>
      </term>
      <term name="in">dans</term>
    </terms>
  </locale>
  <macro name="author">
    <choose>
      <if variable="author">
        <names variable="author">
          <name form="long" and="text" delimiter-precedes-last="never" sort-separator=" "/>
        </names>
      </if>
      <else-if variable="editor">
        <names variable="editor">
          <name form="long" and="text" delimiter-precedes-last="never" sort-separator=" "/>
          <label form="short" prefix="&#160;(" suffix=".)"/>
        </names>
      </else-if>
    </choose>
  </macro>
  <macro name="author-bib">
    <choose>
      <if variable="author">
        <names variable="author">
          <name name-as-sort-order="all" form="long" and="text" delimiter-precedes-last="never" sort-separator=" ">
            <name-part name="family" font-variant="small-caps"/>
          </name>
        </names>
      </if>
      <else-if variable="editor">
        <names variable="editor">
          <name name-as-sort-order="all" form="long" and="text" delimiter-precedes-last="never" sort-separator=" ">
            <name-part name="family" font-variant="small-caps"/>
          </name>
          <label form="short" prefix="&#160;(" suffix=".)"/>
        </names>
      </else-if>
    </choose>
  </macro>
  <macro name="author-ibid">
    <choose>
      <if variable="author">
        <names variable="author">
          <name and="text" initialize="true" initialize-with="." delimiter-precedes-last="never" sort-separator=" " font-style="normal"/>
        </names>
      </if>
      <else-if variable="editor">
        <names variable="editor">
          <name form="long" and="text" delimiter-precedes-last="never" sort-separator=" "/>
          <label form="short" prefix="&#160;(" suffix=".)"/>
        </names>
      </else-if>
    </choose>
  </macro>
  <macro name="editor">
    <names variable="editor">
      <name form="long" and="text" delimiter-precedes-last="never" sort-separator=" "/>
      <label form="short" prefix="&#160;(" suffix=".)"/>
    </names>
  </macro>
  <macro name="translator">
    <names variable="translator">
      <name form="long" and="text" delimiter-precedes-last="never" sort-separator=" " prefix=" traduit par "/>
    </names>
  </macro>
  <macro name="title">
    <choose>
      <if type="bill book graphic legal_case motion_picture report song" match="any">
        <text variable="title" text-case="capitalize-first" font-style="italic"/>
      </if>
      <else-if type="article-journal article-newspaper article-magazine" match="any">
        <group delimiter=", ">
          <text variable="title" text-case="capitalize-first" quotes="true"/>
          <text variable="container-title" font-style="italic"/>
        </group>
      </else-if>
      <else-if type="thesis" match="any">
        <group delimiter="">
          <text variable="title" text-case="capitalize-first" font-style="italic" suffix=","/>
          <text variable="genre" suffix=", " prefix=" "/>
          <text variable="publisher"/>
        </group>
      </else-if>
      <else-if type="manuscript" match="any">
        <group delimiter=",">
          <text variable="title" text-case="capitalize-first" font-style="italic"/>
          <text variable="genre" prefix=" [" suffix="]"/>
        </group>
      </else-if>
      <else-if type="chapter entry-dictionary entry-encyclopedia" match="any">
        <group delimiter="">
          <text variable="title" text-case="capitalize-first" quotes="true"/>
          <text value="dans" suffix=" " prefix=" "/>
          <text macro="editor" suffix=", "/>
          <text variable="container-title" text-case="capitalize-first" font-style="italic"/>
        </group>
      </else-if>
      <else-if type="webpage post-weblog" match="any">
        <group delimiter="">
          <text variable="title" text-case="capitalize-first" font-style="italic" suffix=", "/>
          <text variable="URL"/>
          <group prefix=" , ">
            <date variable="issued">
              <date-part name="day" suffix=" "/>
              <date-part name="month" suffix=" "/>
              <date-part name="year"/>
            </date>
          </group>
        </group>
      </else-if>
      <else>
        <text variable="title" quotes="true"/>
      </else>
    </choose>
  </macro>
  <macro name="pub-place">
    <choose>
      <if type="bill book chapter entry-dictionary entry-encyclopedia thesis graphic legal_case manuscript motion_picture paper-conference report song" match="any">
        <choose>
          <if variable="publisher-place" match="any">
            <text variable="publisher-place"/>
          </if>
          <else>
            <text value="s.l."/>
          </else>
        </choose>
      </if>
    </choose>
  </macro>
  <macro name="publisher">
    <choose>
      <if type="bill book chapter entry-dictionary entry-encyclopedia graphic legal_case motion_picture paper-conference report song" match="any">
        <text variable="publisher"/>
      </if>
    </choose>
  </macro>
  <macro name="yearpage">
    <choose>
      <if type="bill book graphic legal_case motion_picture paper-conference manuscript report song thesis" match="any">
        <group delimiter=", ">
          <date variable="issued">
            <date-part name="year"/>
          </date>
          <group>
            <text term="volume" form="short" suffix="."/>
            <text variable="number-of-volumes" prefix=". " suffix="/"/>
            <text variable="volume"/>
          </group>
          <choose>
            <if variable="locator" match="any">
              <group delimiter="&#8239;">
                <label variable="locator" form="short"/>
                <text variable="locator"/>
              </group>
            </if>
            <else-if variable="locator" match="none">
              <text variable="number-of-pages" suffix="&#160;p"/>
            </else-if>
          </choose>
        </group>
      </if>
      <else-if type="chapter entry-dictionary entry-encyclopedia" match="any">
        <group delimiter=" ">
          <date variable="issued">
            <date-part name="year" suffix=", "/>
          </date>
          <group>
            <text term="volume" form="short" suffix="."/>
            <text variable="number-of-volumes" prefix=". " suffix="/"/>
            <text variable="volume" suffix=","/>
          </group>
          <choose>
            <if variable="locator" match="any">
              <group delimiter="&#8239;">
                <label variable="locator" form="short"/>
                <text variable="locator"/>
              </group>
            </if>
            <else-if variable="locator" match="none">
              <label variable="page" form="short"/>
              <text variable="page"/>
            </else-if>
          </choose>
        </group>
      </else-if>
      <else-if type="article-journal" match="any">
        <group delimiter=" " font-style="normal">
          <choose>
            <if variable="locator" match="any">
              <group delimiter="&#8239;">
                <label variable="locator" form="short"/>
                <text variable="locator"/>
              </group>
            </if>
            <else-if variable="locator" match="none">
              <label variable="page" form="short"/>
              <text variable="page"/>
            </else-if>
          </choose>
        </group>
      </else-if>
      <else-if type="article-newspaper article-magazine" match="any">
        <date variable="issued">
          <date-part name="day" suffix=" "/>
          <date-part name="month" form="short" suffix=" "/>
          <date-part name="year"/>
        </date>
        <group delimiter=" " font-style="normal">
          <label variable="page" form="short"/>
          <text variable="page"/>
        </group>
        <group delimiter=" " font-style="normal">
          <choose>
            <if variable="locator" match="any">
              <group delimiter="&#8239;">
                <label variable="locator" form="short"/>
                <text variable="locator"/>
              </group>
            </if>
            <else-if variable="locator" match="none">
              <label variable="page" form="short"/>
            </else-if>
          </choose>
        </group>
      </else-if>
      <else-if type="webpage post-weblog" match="any">
        <group delimiter=" " prefix="(" suffix=")">
          <text value="consulté le" suffix=" " prefix=" "/>
          <date variable="accessed" form="text">
            <date-part name="day"/>
            <date-part name="month"/>
            <date-part name="year"/>
          </date>
        </group>
      </else-if>
    </choose>
  </macro>
  <macro name="yearpage-bib">
    <choose>
      <if type="bill book graphic legal_case motion_picture paper-conference report song thesis" match="any">
        <group delimiter=", ">
          <group delimiter=", ">
            <date variable="issued">
              <date-part name="year"/>
            </date>
            <group>
              <text term="volume" form="short" suffix="."/>
              <text variable="number-of-volumes" prefix=". " suffix="/"/>
              <text variable="volume"/>
            </group>
            <text variable="number-of-pages" suffix="&#160;p"/>
          </group>
          <group>
            <label variable="locator" form="short"/>
            <text variable="locator"/>
          </group>
        </group>
      </if>
      <else-if type="chapter entry-dictionary entry-encyclopedia" match="any">
        <group delimiter=", ">
          <date variable="issued">
            <date-part name="year"/>
          </date>
          <group>
            <text term="volume" form="short" suffix="."/>
            <text variable="number-of-volumes" prefix=". " suffix="/"/>
            <text variable="volume"/>
          </group>
          <group>
            <label variable="page" form="short"/>
            <text variable="page" prefix="&#160;"/>
          </group>
        </group>
      </else-if>
      <else-if type="article-journal chapter" match="any">
        <group delimiter=" ">
          <label variable="page" form="short"/>
          <text variable="page"/>
        </group>
      </else-if>
      <else-if type="article-newspaper article-magazine" match="any">
        <group delimiter=" ">
          <date variable="issued">
            <date-part name="day" suffix=" "/>
            <date-part name="month" form="short" suffix=" "/>
            <date-part name="year"/>
          </date>
          <label variable="page" form="short"/>
          <text variable="page"/>
        </group>
      </else-if>
      <else-if type="manuscript">
        <group delimiter="" font-style="normal">
          <choose>
            <if variable="issued">
              <date variable="issued">
                <date-part name="day" suffix=" "/>
                <date-part name="month" suffix=" "/>
                <date-part name="year"/>
              </date>
            </if>
            <else>
              <text value="s. d."/>
            </else>
          </choose>
        </group>
      </else-if>
      <else-if type="webpage post-weblog" match="any">
        <group delimiter=" ">
          <text value="consulté le" suffix=" " prefix=" "/>
          <date variable="accessed" form="text">
            <date-part name="day"/>
            <date-part name="month"/>
            <date-part name="year"/>
          </date>
        </group>
      </else-if>
    </choose>
  </macro>
  <macro name="edition">
    <choose>
      <if type="bill book graphic legal_case motion_picture report song chapter paper-conference" match="any">
        <choose>
          <if is-numeric="edition">
            <group delimiter=" ">
              <number variable="edition" form="ordinal"/>
              <text term="edition" form="short"/>
            </group>
          </if>
          <else>
            <text variable="edition" text-case="capitalize-first" suffix="."/>
          </else>
        </choose>
      </if>
      <else-if type="article-journal article-magazine" match="any">
        <group delimiter="">
          <choose>
            <if variable="issued">
              <date variable="issued">
                <date-part name="day" suffix=" "/>
                <date-part name="month" suffix=" "/>
                <date-part name="year"/>
              </date>
              <text macro="volume" prefix=", "/>
            </if>
            <else>
              <text macro="volume" text-case="capitalize-first"/>
            </else>
          </choose>
        </group>
      </else-if>
    </choose>
    <text macro="issue" prefix=", "/>
  </macro>
  <macro name="volume">
    <choose>
      <if is-numeric="volume">
        <text term="volume" form="short" suffix=".&#160;"/>
        <text variable="volume"/>
      </if>
      <else>
        <text variable="volume"/>
      </else>
    </choose>
  </macro>
  <macro name="issue">
    <choose>
      <if is-numeric="issue">
        <text term="issue" form="short" suffix="&#160;"/>
        <text variable="issue"/>
      </if>
      <else>
        <text variable="issue"/>
      </else>
    </choose>
  </macro>
  <macro name="collection">
    <text variable="collection-title" quotes="true" prefix=" (coll.&#160;" suffix=")"/>
  </macro>
  <citation et-al-min="4" et-al-use-first="1">
    <layout suffix="." delimiter="&#160;; ">
      <choose>
        <if position="ibid-with-locator">
          <group delimiter=", ">
            <text term="ibid" text-case="capitalize-first" font-style="italic" suffix="."/>
            <text variable="locator" prefix="p.&#160;"/>
          </group>
        </if>
        <else-if position="ibid">
          <text term="ibid" text-case="capitalize-first" font-style="italic"/>
        </else-if>
        <else-if position="subsequent">
          <group delimiter=", ">
            <text macro="author-ibid"/>
            <choose>
              <if type="bill book graphic legal_case motion_picture report song thesis manuscript" match="any">
                <text variable="title" form="short" font-style="italic"/>
                <text term="cited" font-style="italic" suffix="."/>
              </if>
              <else>
                <text variable="title" text-case="capitalize-first" form="short" quotes="true"/>
                <text value="art cit"/>
              </else>
            </choose>
            <text variable="locator" prefix="p.&#160;"/>
          </group>
        </else-if>
        <else>
          <choose>
            <if type="manuscript">
              <group delimiter=", ">
                <text variable="archive"/>
                <text variable="archive_location"/>
                <text variable="call-number"/>
                <text macro="title"/>
                <text macro="yearpage-bib"/>
              </group>
            </if>
            <else-if type="bill chapter article-journal article-newspaper interview book graphic legal_case motion_picture paper-conference report song thesis webpage post-weblog article-magazine" match="any">
              <group delimiter=", ">
                <text macro="author"/>
                <text macro="title"/>
                <text macro="translator"/>
                <text macro="edition"/>
                <text macro="pub-place"/>
                <text macro="publisher"/>
                <text macro="yearpage"/>
              </group>
            </else-if>
          </choose>
        </else>
      </choose>
    </layout>
  </citation>
  <bibliography>
    <sort>
      <key macro="author" names-min="3" names-use-first="3"/>
      <key variable="issued" sort="descending"/>
    </sort>
    <layout suffix=".">
      <choose>
        <if type="manuscript">
          <group delimiter=", ">
            <text variable="archive"/>
            <text variable="archive_location"/>
            <text variable="call-number"/>
            <text macro="title"/>
            <text macro="yearpage-bib"/>
          </group>
        </if>
        <else-if type="bill chapter article-journal article-newspaper interview book graphic legal_case motion_picture paper-conference report song thesis webpage post-weblog article-magazine" match="any">
          <group delimiter=", ">
            <text macro="author-bib"/>
            <text macro="title"/>
            <text macro="translator"/>
            <text macro="edition"/>
            <text macro="pub-place"/>
            <group delimiter=" ">
              <text macro="publisher"/>
              <text macro="collection"/>
            </group>
            <text macro="yearpage-bib"/>
          </group>
        </else-if>
      </choose>
    </layout>
  </bibliography>
</style>` }
    },

  ["LaTeX to DocBook with MathML"]:
  { text: `\\newtheorem{theorem}{Theorem}
\\newtheorem{corollary}[theorem]{Corollary}
\\newtheorem{lemma}[theorem]{Lemma}
\\theoremstyle{definition}
\\newtheorem{definition}[theorem]{Definition}
\\theoremstyle{remark}
\\newtheorem{remark}{Remark}

\\begin{definition}[right-angled triangles] \\label{def:tri}
A \\emph{right-angled triangle} is a triangle whose sides of length~\\(a\\), \\(b\\) and~\\(c\\), in some permutation of order, satisfies \\(a^2+b^2=c^2\\).
\\end{definition}

\\begin{lemma}
The triangle with sides of length~\\(3\\), \\(4\\) and~\\(5\\) is right-angled.
\\end{lemma}

\\begin{proof}
This lemma follows from \\cref{def:tri} since \\(3^2+4^2=9+16=25=5^2\\).
\\end{proof}

\\begin{theorem}[Pythagorean triplets] \\label{thm:py}
Triangles with sides of length \\(a=p^2-q^2\\), \\(b=2pq\\) and \\(c=p^2+q^2\\) are right-angled triangles.
\\end{theorem}

\\begin{remark}
These are all pretty interesting facts.
\\end{remark}
`,
    from: 'latex',
    to: 'docbook5',
    ['html-math-method']: 'mathml',
    standalone: true },

  ["Custom template"]:
  { text: `---
keywords:
- bee
- ant
- ladybug
author: E. N. Tymologist
title: Some bugs
...

This is a book about bugs.`,
    from: 'markdown',
    to: 'html5',
    standalone: true,
    template: `<h1>$title$</h1>
<p>by $author$</p>
<p>Keywords: $for(keywords)$$it$$sep$; $endfor$</p>
<main>
$body$
</main>
` },

  ["ipynb to rtf"]:
  { text: `{
 "cells": [
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "# Lorem ipsum\\n",
    "\\n",
    "**Lorem ipsum** dolor sit amet, consectetur adipiscing elit. Nunc luctus\\n",
    "bibendum felis dictum sodales."
   ],
   "id": "42a14256-91c8-446e-92a7-ab6bf11055d3"
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "print(\\"hello\\")"
   ],
   "id": "98ee7437-e11a-4c16-b642-9e7911f32cd2"
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## Pyout"
   ],
   "id": "2df739f6-1afd-400b-845c-ad1efebc209f"
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "from IPython.display import HTML\\n",
    "HTML(\\"\\"\\"\\n",
    "<script>\\n",
    "console.log(\\"hello\\");\\n",
    "</script>\\n",
    "<b>HTML</b>\\n",
    "\\"\\"\\")"
   ],
   "id": "622b77f5-76e7-46cb-a694-56007ed6adbe"
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## Image\\n",
    "\\n",
    "This image ![the moon](attachment:lalune.jpg) will be included as a cell\\n",
    "attachment."
   ],
   "attachments": {
    "lalune.jpg": {
     "image/jpeg": "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\\n3FNQTvPJ7UYAelf/2Q==\\n"
    }
   },
   "id": "0db42901-5dc0-4fd3-b2db-b83b3c584437"
  }
 ],
 "nbformat": 4,
 "nbformat_minor": 5,
 "metadata": {}
}`,
  from: 'ipynb',
  to: 'rtf',
  standalone: true },

  ["docx with equations to LaTeX"]:
  { text: `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`,
  from: 'docx',
  to: 'latex',
  standalone: true }
};


