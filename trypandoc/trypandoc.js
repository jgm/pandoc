"use strict";

const defaultParams = {
  text: '',
  to: 'html5',
  from: 'markdown',
  standalone: false,
  citeproc: false,
  files: {} };

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
    to: 'org' }

}

var params = examples["Hello world"];

function clearText() {
  params.text = '';
  document.getElementById("text").value = '';
}

function addFile(name, contents) {
  params.files[name] = contents;
  let filesDiv = document.getElementById("files");
  let fileDiv = document.createElement("div");
  fileDiv.classList.add("file");
  let title = document.createElement("div");
  title.classList.add("title");
  let removeButton = document.createElement("button");
  removeButton.textContent = "Remove";
  removeButton.onclick = (e) => {
    delete params.files[name];
    e.target.parentElement.parentElement.remove();
  }
  let filename = document.createElement("span");
  filename.classList.add("filename");
  filename.textContent = name;
  title.appendChild(filename);
  title.appendChild(removeButton);
  fileDiv.appendChild(title);
  let textarea = document.createElement("textarea");
  textarea.onchange = (e) => {
    params.files[name] = e.target.value;
  }
  textarea.textContent = contents;
  fileDiv.appendChild(textarea);
  filesDiv.appendChild(fileDiv);
}

function permalink() {
  let href = window.location.href;
  const URLparams = new URLSearchParams([["params", JSON.stringify(params)]]);
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

function paramsFromURL() {
  if (window.location.search.length > 0) {
    const query = new URLSearchParams(window.location.search);
    const rawparams = query.get("params");
    params = JSON.parse(rawparams);
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
}

function setFormFromParams() {
    document.getElementById("text").value = params.text;
    document.getElementById("from").value = params.from;
    document.getElementById("to").value = params.to;
    document.getElementById("standalone").checked = params.standalone;
    document.getElementById("citeproc").checked = params.citeproc;
    for (const filename in params.files) {
      addFile(filename, params.files[filename]);
    }
}

function readFile(file, callback) {
    if (file.size > 200000) {
      alert("File exceeds 200KB size limit: " + file.name);
      throw("File exceeds 200KB size limit: " + file.name);
    }
    const reader = new FileReader();
    reader.onloadend = () => {
      let result = reader.result;
      // check for valid UTF-8
      let invalidUtf8 = result.match(/[\uFFFD]/);
      if (invalidUtf8) {
        // if not valid UTF-8, treat as binary and base64 encode it
        const base64reader = new FileReader();
        base64reader.onloadend = () => {
          const base64string = base64reader.result
           .replace('data:', '')
           .replace(/^.+,/, '');
          callback(base64string);
        }
        base64reader.readAsDataURL(file);
      } else {
        callback(result);
      }
    }
    reader.readAsText(file);
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
      let newparams = examples[e.target.value];
      params = defaultParams;
      for (const key in newparams) {
        params[key] = newparams[key]; // allow defaults
      };
      setFormFromParams();
      convert();
    }

    const fileInput = document.getElementById('loadfile');

    // Listen for the change event so we can capture the file
    fileInput.addEventListener('change', (e) => {
      // Get a reference to the file
      let inputtext = document.getElementById("text");
      const file = e.target.files[0];
      readFile(file, (s) => {
        inputtext.value = s;
        params.text = s;
      });
    });

    const addfileButton = document.getElementById("addfile");
    addfileButton.addEventListener('change', (e) => {
      // Get a reference to the file
      const file = e.target.files[0];
      readFile(file, (s) => {
        addFile(file.name, s);
      });
    });

    fetch("/cgi-bin/pandoc-server.cgi/version")
       .then(handleErrors)
       .then(response => response.text())
       .then(restext =>
           document.getElementById("version").textContent = restext
         );

    convert();

})();

