"use strict";

// reset params to defaults
function resetParams() {
  params.text = '';
  params.to = 'html5';
  params.from = 'markdown';
  params.standalone = false;
  params.citeproc = false;
  params["html-math-method"] = "plain";
  params.files = {};
};

var params = {};

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
    document.getElementById("html-math-method").value = params["html-math-method"];
    const files = document.querySelectorAll(".file");
    files.forEach(file => {
      file.remove();
    });
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
    resetParams();
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
    // document.getElementById("text").onblur = (e) => {
    //   convert();
    // }
    document.getElementById("standalone").onchange = (e) => {
      params.standalone = e.target.checked;
      convert();
    }
    document.getElementById("citeproc").onchange = (e) => {
      params.citeproc = e.target.checked;
      convert();
    }
    document.getElementById("html-math-method").onchange = (e) => {
      params["html-math-method"] = e.target.value;
      convert();
    }

    document.getElementById("examples").onchange = (e) => {
      let newparams = examples[e.target.value];
      resetParams();
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

