"use strict";

var params = {
  text: '"hello *world*"',
  to: 'html5',
  from: 'markdown',
  standalone: false,
  citeproc: false,
  files: {} };

const examples = {
  ["Hello world"]:
    { text: '*Hello* world!',
      from: 'markdown',
      to: 'html5',
      standalone: false,
      citeproc: false,
      files: {} },
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
      to: 'csljson',
      standalone: false,
      citeproc: false,
      files: {} },
  ["Markdown to man with citations"]:
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
    to: 'man',
    standalone: false,
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
    standalone: true,
    citeproc: false,
    files: {} },
  ["HTML to reStructuredText"]:
  { text: `<h2 class="options" id="reader-options">Reader options</h2>
<dl>
<dt><code>-L</code> <em>SCRIPT</em>,
<code>--lua-filter=</code><em>SCRIPT</em></dt>
<dd>
<p>Transform the document in a similar fashion as JSON filters (see
<code>--filter</code>), but use pandoc’s built-in Lua filtering system.
The given Lua script is expected to return a list of Lua filters which
will be applied in order. Each Lua filter must contain
element-transforming functions indexed by the name of the AST element on
which the filter function should be applied.</p>
<p>The <code>pandoc</code> Lua module provides helper functions for
element creation. It is always loaded into the script’s Lua
environment.</p>
<p>See the <a href="https://pandoc.org/lua-filters.html">Lua filters
documentation</a> for further details.</p>
<p>In order of preference, pandoc will look for Lua filters in</p>
<ol type="1">
<li><p>a specified full or relative path,</p></li>
<li><p><code>$DATADIR/filters</code> where <code>$DATADIR</code> is the
user data directory (see <code>--data-dir</code>, above).</p></li>
</ol>
<p>Filters, Lua filters, and citeproc processing are applied in the
order specified on the command line.</p>
</dd>
<dt><code>--trace</code></dt>
<dd>
<p>Print diagnostic output tracing parser progress to stderr. This
option is intended for use by developers in diagnosing performance
issues.</p>
</dd>
</dl>`,
    from: 'html',
    to: 'rst',
    standalone: false,
    citeproc: false,
    files: {} },
  ["LaTeX with macros to DocBook"]:
  { text: `% from https://en.wikibooks.org/wiki/LaTeX/Macros
\\newcommand{\\wbalTwo}[2][Wikimedia]{
This is the Wikibook about LaTeX
supported by {#1} and {#2}!}

\\begin{itemize}
\\item \\wbalTwo{John Doe}
\\item \\wbalTwo[lots of users]{John Doe}
\\end{itemize}`,
    from: 'latex',
    to: 'docbook5',
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
    to: 'org',
    standalone: false,
    citeproc: false,
    files: {} }

}

function permalink() {
  let href = window.location.href;
  const URLparams = new URLSearchParams(Object.entries(params));
  return href.replace(/([?].*)?$/,"?" + URLparams);
}

const binaryFormats = {
   docx: { extension: "docx",
           mime: "application/vnd.openxmlformats-officedocument.wordprocessingml.document" },
    odt: { extension: "odt",
           mime: "application/vnd.oasis.opendocument.text" },
    pptx: { extension: "pptx",
            mime: "application/vnd.openxmlformats-officedocument.presentationml.presentation" },
    epub:  { extension: "epub",
             mime: "application/epub+zip" },
    epub2: { extension: "epub",
             mime: "application/epub+zip" },
    epub3: { extension: "epub",
             mime: "application/epub+zip" }
};

const binaryMimeTypes = {
  ["application/epub+zip"]: true,
  ["application/vnd.openxmlformats-officedocument.wordprocessingml.document"]: true,
  ["application/vnd.openxmlformats-officedocument.presentationml.presentation"]: true,
  ["application/vnd.oasis.opendocument.text"]: true
};

function paramsFromURL() {
  if (window.location.search.length > 0) {
    const uparams = new URLSearchParams(window.location.search);
    params.text = uparams.get("text") || "";
    params.from = uparams.get("from") || "markdown";
    params.to = uparams.get("to") || "html5";
    params.standalone = uparams.get("standalone") === "true";
    params.citeproc = uparams.get("citeproc") === "true";
  }
}

function handleErrors(response) {
    let errs = document.getElementById("errors");
    if (!response.ok) {
      errs.textContent = "Conversion failed, status = " + response.status;
      errs.style.display = "block";
    }
    if (response.status == 503) {
      errs.textContent += "  Timed out.";
    }
    return response;
}

function convert() {
    document.getElementById("results").textContent = "";
    let errs = document.getElementById("errors");
    errs.style.display = "none";
    errs.textContent = "";
    console.log(params);

    if (params.text && params.text != "") {
       let commandString = "pandoc"
         + " --from " + params.from + " --to " + params.to
         + (params.standalone ? " --standalone" : "")
         + (params.citeproc ? " --citeproc" : "") ;
       document.getElementById("command").textContent = commandString;
       fetch("/cgi-bin/pandoc-server.cgi", {
         method: "POST",
         headers: {"Content-Type": "application/json"},
         body: JSON.stringify(params)
        })
       .then(handleErrors)
       .then(response => response.text())
       .then(restext => {
            let binary = binaryFormats[params.to];
            if (binary &&
              document.getElementById("errors").style.display == "none") {
            document.getElementById("results").innerHTML +=
                '<a download="trypandoc.' + binary.extension +
                '" href="data:' + binary.mime + ';base64,' + restext +
                '">click to download trypandoc.' + binary.extension + '</a>';
          } else {
            document.getElementById("results").textContent += restext;
          }
          document.getElementById("permalink").href = permalink();
       });
    };
}

function setFormFromParams() {
    document.getElementById("text").value = params.text;
    document.getElementById("from").value = params.from;
    document.getElementById("to").value = params.to;
    document.getElementById("standalone").checked = params.standalone;
    document.getElementById("citeproc").checked = params.citeproc;
}


(function() {
    paramsFromURL();
    setFormFromParams();

    const exampleSelect = document.getElementById("examples");
    for (const k in examples) {
      exampleSelect.innerHTML += '<option value="' + k + '">' + k + '</option>';
    }

    document.getElementById("convert").onclick = convert;
    document.getElementById("from").onchange = (e) => {
      params.from = e.target.value;
      convert();
    }
    document.getElementById("to").onchange = (e) => {
      params.to = e.target.value;
      convert();
    }
    document.getElementById("text").onchange = (e) => {
      params.text = e.target.value;
    }
    document.getElementById("standalone").onchange = (e) => {
      params.standalone = e.target.checked;
      convert();
    }
    document.getElementById("citeproc").onchange = (e) => {
      params.citeproc = e.target.checked;
      convert();
    }

    document.getElementById("examples").onchange = (e) => {
      params = examples[e.target.value];
      setFormFromParams();
      convert();
    }

    const fileInput = document.getElementById('loadfile');

    // Listen for the change event so we can capture the file
    fileInput.addEventListener('change', (e) => {
      // Get a reference to the file
      const file = e.target.files[0];
      const mimetype = file.type;
      let binary = binaryMimeTypes[mimetype];

      // Encode the file using the FileReader API
      const reader = new FileReader();
      let inputtext = document.getElementById("text");
      reader.onloadend = () => {
        // Use a regex to remove data url part
        if (binary) {
          const base64String = reader.result
           .replace('data:', '')
           .replace(/^.+,/, '');
          inputtext.value = base64String;
	} else {
          inputtext.value = reader.result;
        }
        params.text = inputtext.value;
      };
      if (binary) {
        reader.readAsDataURL(file);
      } else {
        reader.readAsText(file);
      }
    });

    // const supportFiles = document.getElementById('supportfiles');
    //
    // // Listen for the change event so we can capture the file
    // supportFiles.addEventListener('change', (e) => {
    //   // Get a reference to the file
    //   const files = e.target.files;
    //   params.files = {};
    //   Object.keys(files).forEach(i => {
    //     const file = files[i];
    //     const reader = new FileReader();
    //     reader.onload = (e) => {
    //       params.files[file.name] = reader.result
    //        .replace('data:', '')
    //        .replace(/^.+,/, '');
    //     }
    //     reader.readAsDataURL(file);
    //   });
    // });

    fetch("/cgi-bin/pandoc-server.cgi/version")
       .then(handleErrors)
       .then(response => response.text())
       .then(restext =>
           document.getElementById("version").textContent = restext
         );

    convert();

})();

