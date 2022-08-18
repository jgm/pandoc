"use strict";

var params = {
  text: '"hello *world*"',
  to: 'html5',
  from: 'markdown',
  standalone: false,
  citeproc: false };

function permalink() {
  let input = document.getElementById("text").value;
  let from = document.getElementById("from").value;
  let to = document.getElementById("to").value;
  let standalone = document.getElementById("standalone").checked ? true : false;
  let citeproc = document.getElementById("citeproc").checked ? true : false;
  let href = window.location.href;
  const URLparams = new URLSearchParams(Object.entries({text: input, from: from, to: to, standalone: standalone, citeproc: citeproc}));
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
    if (!response.ok) {
        throw Error(response.statusText);
    }
    return response;
}

function convert() {
    let text = document.getElementById("text").value;
    let from = document.getElementById("from").value;
    let to = document.getElementById("to").value;
    let standalone = document.getElementById("standalone").checked;
    let citeproc = document.getElementById("citeproc").checked;
    params = { text: text, from: from, to: to, standalone: standalone,
               citeproc: citeproc };

    if (text && text != "") {
       fetch("/cgi-bin/pandoc-server.cgi/version")
          .then(handleErrors)
          .catch(error =>
            document.getElementById("results").textContent = error
            )
          .then(response => response.text())
          .then(restext =>
              document.getElementById("version").textContent = restext
            );

       // console.log(JSON.stringify(params));
       let commandString = "pandoc"
         + " --from " + from + " --to " + to
         + (standalone ? " --standalone" : "")
         + (citeproc ? " --citeproc" : "") ;
       document.getElementById("command").textContent = commandString;
       fetch("/cgi-bin/pandoc-server.cgi", {
         method: "POST",
         headers: {"Content-Type": "application/json"},
         body: JSON.stringify(params)
        })
       .then(response => response.text())
       .then(restext => {
            let binary = binaryFormats[to];
            if (binary) {
            document.getElementById("results").innerHTML =
                '<a download="trypandoc.' + binary.extension +
                '" href="data:' + binary.mime + ';base64,' + restext +
                '">click to download trypandoc.' + binary.extension + '</a>';
          } else {
            document.getElementById("results").textContent = restext;
          }
          document.getElementById("permalink").href = permalink();
       });
    };
}

(function() {
    paramsFromURL();
    document.getElementById("text").value = params.text;
    document.getElementById("from").value = params.from;
    document.getElementById("to").value = params.to;
    document.getElementById("standalone").checked = params.standalone;
    document.getElementById("citeproc").checked = params.citeproc;

    document.getElementById("convert").onclick = convert;
    document.getElementById("from").onchange = convert;
    document.getElementById("to").onchange = convert;
    document.getElementById("standalone").onchange = convert;
    document.getElementById("citeproc").onchange = convert;

    document.getElementById("examples").onchange =
      (e => window.location.href = e.target.value );

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
        console.log(mimetype);
        if (binary) {
          const base64String = reader.result
           .replace('data:', '')
           .replace(/^.+,/, '');
          inputtext.value = base64String;
	} else {
          inputtext.value = reader.result;
        }
      };
      if (binary) {
        reader.readAsDataURL(file);
      } else {
        reader.readAsText(file);
      }
    });

    convert();

})();

