import { zipSync, unzipSync, strToU8, strFromU8 } from 'https://esm.sh/fflate@0.8.2';

// Make fflate available globally
window.fflate = { zipSync, unzipSync, strToU8, strFromU8 };

// Pandoc loading - starts immediately in background, but doesn't block UI
let pandocReadyPromise = null;
let onFormatsLoaded = null; // Callback to update Vue app when formats load

function loadPandoc() {
  // Only load once
  if (pandocReadyPromise) return pandocReadyPromise;

  pandocReadyPromise = (async () => {
    // Dynamic import so WASM loading happens in background
    const { convert, query } = await import("./pandoc.js?sha1=SHA1_PANDOC_JS");
    window.pandocModule = { convert, query };

    // Query version and formats
    const pandocVersion = await query({ query: "version" });
    document.getElementById("pandoc-version").innerText = pandocVersion;

    const inputFormats = await query({ query: "input-formats" });
    const outputFormats = await query({ query: "output-formats" });

    // Update Vue app if callback is registered
    if (onFormatsLoaded) {
      onFormatsLoaded(inputFormats, outputFormats);
    }
  })();

  return pandocReadyPromise;
}

// Start loading immediately in background (fire and forget)
loadPandoc().catch(err => console.error("Failed to load pandoc:", err));

// Lazy-load typst library only when needed
let typstLoaded = false;
let typstLoadingPromise = null;

window.loadTypst = async function() {
  if (typstLoaded && typeof $typst !== 'undefined') return;
  if (typstLoadingPromise) return typstLoadingPromise;

  typstLoadingPromise = new Promise((resolve, reject) => {
    const typstScript = document.createElement('script');
    typstScript.type = 'module';
    typstScript.src = 'https://cdn.jsdelivr.net/npm/@myriaddreamin/typst-all-in-one.ts@0.7.0-rc2/dist/esm/index.js';
    typstScript.onload = () => {
      const checkTypst = () => {
        if (typeof $typst !== 'undefined') {
          typstLoaded = true;
          resolve();
        } else {
          setTimeout(checkTypst, 100);
        }
      };
      checkTypst();
    };
    typstScript.onerror = () => reject(new Error('Failed to load Typst library'));
    document.head.appendChild(typstScript);
  });
  return typstLoadingPromise;
};

// Petite Vue app definition
window.pandocApp = function() {
  return {
    // State
    pandocReady: false,
    waitingForPandoc: false,
    selectedExample: '',
    showExamplesBar: false,
    inputMode: 'file',
    files: {},
    fileOrder: [],
    auxFiles: {},
    textInput: '',
    inputFormat: 'auto',
    outputFormat: 'auto',
    outputFilename: '',
    activeTab: 'general',
    dragOver: false,
    fileDraggingIdx: -1,
    fileDragOverIdx: -1,

    // Options
    opts: {
      standalone: true,
      toc: false,
      numberSections: false,
      fileScope: false,
      extractMedia: false,
      tocDepth: '3',
      shiftHeading: '0',
      tabStop: '4',
      preserveTabs: false,
      eol: '',
      dpi: '',
      numberOffset: '',
      resourcePath: '',
      defaultImageExtension: '',
      stripComments: false,
      trackChanges: '',
      citationMethod: '',
      mathMethod: '',
      highlightStyle: '',
      indentedCodeClasses: '',
      selfContained: false,
      htmlQTags: false,
      sectionDivs: false,
      asciiHtml: false,
      titlePrefix: '',
      idPrefix: '',
      emailObfuscation: '',
      referenceLinks: false,
      referenceLocation: '',
      markdownHeadings: '',
      referenceLinksRst: false,
      listTables: false,
      lof: false,
      lot: false,
      figureCaptionPosition: '',
      tableCaptionPosition: '',
      topLevelDivision: '',
      ascii: false,
      epubTitlePage: true,
      epubSubdirectory: '',
      splitLevel: '',
      chunkTemplate: '',
      ipynbOutput: '',
      slideLevel: '',
      incremental: false,
      wrap: 'auto',
      columns: '',
      linkImages: false
    },

    // Metadata
    customMetadata: [{ key: '', value: '' }],
    customVariables: [{ key: '', value: '' }],

    // Resource files
    bibFile: null,
    cslFile: null,
    abbrevFile: null,
    abbreviationsFile: null,
    cssFiles: [],
    highlightThemeFile: null,
    syntaxDefinitions: [],
    templateFile: null,
    referenceDoc: null,
    metadataFile: null,
    epubCoverImage: null,
    epubMetadataFile: null,
    epubFonts: [],
    headerFiles: [],
    beforeBodyFiles: [],
    afterBodyFiles: [],
    luaFilters: [],
    filterDraggingIdx: -1,
    filterDragOverIdx: -1,

    // CSL
    cslStyleName: '',
    cslStylesList: [],
    cslStylesLoaded: false,

    // Extensions
    inputExtensions: {},
    outputExtensions: {},
    inputExtensionsList: [],
    outputExtensionsList: [],

    // Output
    hasChanges: false,
    isConverting: false,
    showOutput: false,
    output: null,
    outputFilenameActual: '',
    outputPreview: '',
    messages: [],
    verbosity: 'info',
    copyBtnText: '📋 Copy to Clipboard',
    mediaZip: null,

    // Format names: maps format ID to human-readable name
    // Some formats have different names for input vs output
    formatNames: {
      ansi: 'ANSI terminal',
      asciidoc: 'modern AsciiDoc',
      asciidoc_legacy: 'AsciiDoc for asciidoc-py',
      asciidoctor: 'AsciiDoctor (= modern AsciiDoc)',
      bbcode: 'BBCode',
      beamer: 'LaTeX Beamer slides',
      biblatex: 'BibLaTeX bibliography',
      bibtex: 'BibTeX bibliography',
      bits: 'BITS XML, alias for jats',
      chunkedhtml: 'zip of linked HTML files',
      commonmark: 'CommonMark Markdown',
      commonmark_x: 'CommonMark with extensions',
      context: 'ConTeXt',
      creole: 'Creole 1.0',
      csljson: 'CSL JSON bibliography',
      csv: 'CSV table',
      djot: 'Djot markup',
      docbook: 'DocBook v4',
      docbook5: 'DocBook v5',
      docx: 'Word',
      dokuwiki: 'DokuWiki markup',
      dzslides: 'DZSlides HTML slides',
      endnotexml: 'EndNote XML bibliography',
      epub: 'EPUB v3',
      epub2: 'EPUB v2',
      epub3: 'EPUB v3',
      fb2: 'FictionBook2',
      gfm: 'GitHub-Flavored Markdown',
      haddock: 'Haddock markup',
      html: 'HTML',
      html4: 'XHTML 1.0 Transitional',
      html5: 'HTML',
      icml: 'InDesign ICML',
      ipynb: 'Jupyter notebook',
      jats: 'JATS XML',
      jira: 'Jira/Confluence wiki markup',
      json: 'JSON version of native AST',
      latex: 'LaTeX',
      man: 'roff man',
      markdown: "Pandoc's Markdown",
      markdown_mmd: 'MultiMarkdown',
      markdown_phpextra: 'PHP Markdown Extra',
      markdown_strict: 'original unextended Markdown',
      markua: 'Markua',
      mdoc: 'mdoc manual page markup',
      mediawiki: 'MediaWiki markup',
      ms: 'roff ms',
      muse: 'Muse',
      native: 'native Haskell',
      odt: 'OpenDocument text',
      opendocument: 'OpenDocument XML',
      opml: 'OPML',
      org: 'Emacs Org mode',
      pdf: 'PDF via Typst',
      plain: 'plain text',
      pod: 'Perl POD',
      pptx: 'PowerPoint',
      revealjs: 'reveal.js HTML slides',
      ris: 'RIS bibliography',
      rst: 'reStructuredText',
      rtf: 'Rich Text Format',
      s5: 'S5 HTML slides',
      slideous: 'Slideous HTML slides',
      slidy: 'Slidy HTML slides',
      t2t: 'txt2tags',
      tei: 'TEI Simple',
      texinfo: 'GNU Texinfo',
      textile: 'Textile',
      tikiwiki: 'TikiWiki markup',
      tsv: 'TSV table',
      twiki: 'TWiki markup',
      typst: 'Typst',
      vimdoc: 'Vimdoc',
      vimwiki: 'Vimwiki',
      xlsx: 'Excel spreadsheet',
      xml: 'XML version of native AST',
      xwiki: 'XWiki markup',
      zimwiki: 'ZimWiki markup'
    },

    // List of supported formats (populated when pandoc loads)
    inputFormats: [],
    outputFormats: [],
    formatsLoaded: false,

    // Format constants
    formatByExtension: {
      'md': 'markdown', 'markdown': 'markdown', 'mkd': 'markdown',
      'html': 'html', 'htm': 'html',
      'tex': 'latex', 'latex': 'latex',
      'rst': 'rst', 'org': 'org', 'docx': 'docx', 'odt': 'odt',
      'epub': 'epub', 'txt': 'markdown', 'json': 'json', 'ipynb': 'ipynb',
      'xml': 'docbook', 'wiki': 'mediawiki', 'textile': 'textile',
      'rtf': 'rtf', 'bib': 'bibtex', 'csv': 'csv', 'tsv': 'tsv',
      'typ': 'typst', 'typst': 'typst', 'pptx': 'pptx'
    },

    extensionByFormat: {
      'html': 'html', 'html5': 'html', 'html4': 'html', 'chunkedhtml': 'zip',
      'markdown': 'md', 'markdown_strict': 'md', 'markdown_mmd': 'md', 'markdown_phpextra': 'md',
      'gfm': 'md', 'commonmark': 'md', 'commonmark_x': 'md',
      'latex': 'tex', 'beamer': 'tex', 'context': 'tex',
      'pdf': 'pdf', 'docx': 'docx', 'odt': 'odt',
      'epub': 'epub', 'epub2': 'epub', 'epub3': 'epub',
      'rst': 'rst', 'org': 'org', 'plain': 'txt',
      'json': 'json', 'native': 'native',
      'docbook': 'xml', 'docbook4': 'xml', 'docbook5': 'xml',
      'jats': 'xml', 'tei': 'xml', 'man': '1', 'rtf': 'rtf',
      'textile': 'textile', 'mediawiki': 'wiki',
      'asciidoc': 'adoc', 'asciidoctor': 'adoc', 'asciidoc_legacy': 'adoc',
      'revealjs': 'html', 'slidy': 'html', 'slideous': 'html', 'dzslides': 'html', 's5': 'html',
      'ipynb': 'ipynb', 'typst': 'typ', 'texinfo': 'texi', 'ms': 'ms', 'icml': 'icml',
      'opml': 'opml', 'bibtex': 'bib', 'biblatex': 'bib', 'csljson': 'json',
      'pptx': 'pptx', 'djot': 'dj', 'fb2': 'fb2', 'opendocument': 'xml', 'vimdoc': 'txt'
    },

    slideFormats: ['revealjs', 'slidy', 'slideous', 'dzslides', 's5', 'beamer', 'pptx'],
    htmlFormats: ['html', 'html4', 'html5', 'revealjs', 'slidy', 'slideous', 'dzslides', 's5', 'epub', 'epub2', 'epub3', 'chunkedhtml'],
    docFormats: ['docx', 'odt', 'pptx'],
    binaryFormats: ['docx', 'odt', 'pptx', 'epub', 'epub2', 'epub3', 'pdf', 'chunkedhtml'],
    markdownFormats: ['markdown', 'markdown_strict', 'markdown_mmd', 'markdown_phpextra', 'gfm', 'commonmark', 'commonmark_x'],
    captionPositionFormats: ['html', 'html4', 'html5', 'latex', 'beamer', 'docx', 'odt', 'typst', 'pdf'],
    asciiFormats: ['html', 'html4', 'html5', 'markdown', 'markdown_strict', 'markdown_mmd', 'markdown_phpextra', 'gfm', 'commonmark', 'commonmark_x', 'docbook', 'docbook4', 'docbook5', 'jats', 'man', 'ms', 'latex', 'beamer'],
    topLevelDivisionFormats: ['latex', 'beamer', 'context', 'docbook', 'docbook4', 'docbook5', 'tei'],
    listOfFormats: ['latex', 'beamer', 'context'],
    latexCitationFormats: ['latex', 'beamer', 'context'],

    // Template variables by category
    templateVariablesByCategory: {
      all: ['title', 'author', 'date', 'subtitle', 'abstract', 'abstract-title', 'keywords', 'subject', 'description', 'category', 'lang', 'dir', 'header-includes', 'include-before', 'include-after', 'toc-title'],
      html: ['document-css', 'mainfont', 'fontsize', 'fontcolor', 'linkcolor', 'monofont', 'monobackgroundcolor', 'linestretch', 'maxwidth', 'backgroundcolor', 'margin-left', 'margin-right', 'margin-top', 'margin-bottom'],
      htmlSlides: ['institute', 'revealjs-url', 's5-url', 'slidy-url', 'slideous-url', 'title-slide-attributes'],
      beamer: ['aspectratio', 'beameroption', 'institute', 'logo', 'navigation', 'section-titles', 'theme', 'colortheme', 'fonttheme', 'innertheme', 'outertheme', 'themeoptions', 'titlegraphic'],
      latex: ['block-headings', 'classoption', 'documentclass', 'geometry', 'hyperrefoptions', 'indent', 'linestretch', 'margin-left', 'margin-right', 'margin-top', 'margin-bottom', 'pagestyle', 'papersize', 'secnumdepth', 'fontenc', 'fontfamily', 'fontfamilyoptions', 'fontsize', 'mainfont', 'sansfont', 'monofont', 'mathfont', 'CJKmainfont', 'mainfontoptions', 'sansfontoptions', 'monofontoptions', 'colorlinks', 'linkcolor', 'filecolor', 'citecolor', 'urlcolor', 'toccolor', 'links-as-notes', 'lof', 'lot', 'thanks', 'toc-depth', 'biblatexoptions', 'biblio-style', 'biblio-title', 'natbiboptions'],
      context: ['fontsize', 'headertext', 'footertext', 'indenting', 'interlinespace', 'layout', 'linkcolor', 'contrastcolor', 'linkstyle', 'lof', 'lot', 'mainfont', 'sansfont', 'monofont', 'mathfont', 'margin-left', 'margin-right', 'margin-top', 'margin-bottom', 'pagenumbering', 'papersize', 'whitespace'],
      typst: ['papersize', 'mainfont', 'fontsize', 'section-numbering', 'page-numbering', 'columns', 'thanks', 'mathfont', 'codefont', 'linestretch', 'linkcolor', 'filecolor', 'citecolor', 'margin'],
      ms: ['fontfamily', 'indent', 'lineheight', 'pointsize'],
      man: ['adjusting', 'footer', 'header', 'section'],
      epub: ['cover-image', 'epub-title-page'],
      docx: ['reference-doc'],
      pptx: ['monofont'],
      odt: ['reference-doc']
    },

    // Format to variable categories mapping
    formatVariableCategories: {
      html: ['all', 'html'],
      html4: ['all', 'html'],
      html5: ['all', 'html'],
      revealjs: ['all', 'html', 'htmlSlides'],
      slidy: ['all', 'html', 'htmlSlides'],
      slideous: ['all', 'html', 'htmlSlides'],
      dzslides: ['all', 'html', 'htmlSlides'],
      s5: ['all', 'html', 'htmlSlides'],
      epub: ['all', 'html', 'epub'],
      epub2: ['all', 'html', 'epub'],
      epub3: ['all', 'html', 'epub'],
      chunkedhtml: ['all', 'html'],
      latex: ['all', 'latex'],
      beamer: ['all', 'latex', 'beamer'],
      context: ['all', 'context'],
      typst: ['all', 'typst'],
      pdf: ['all', 'typst'],
      ms: ['all', 'ms'],
      man: ['all', 'man'],
      docx: ['all', 'docx'],
      pptx: ['all', 'pptx'],
      odt: ['all', 'odt'],
      opendocument: ['all', 'odt']
    },

    // Computed-like getters
    get canConvert() {
      const hasInput = this.inputMode === 'file' ? this.fileOrder.length > 0 : this.textInput.trim().length > 0;
      return hasInput && this.hasChanges;
    },

    get effectiveOutputFormat() {
      if (this.outputFormat !== 'auto') return this.outputFormat;
      const outFile = this.outputFilename.trim() || this.outputFilenamePlaceholder;
      if (outFile && outFile !== '(preview)') {
        const ext = outFile.split('.').pop().toLowerCase();
        const formatByOutputExtension = {
          'html': 'html', 'htm': 'html', 'md': 'markdown', 'markdown': 'markdown',
          'tex': 'latex', 'pdf': 'pdf', 'docx': 'docx', 'odt': 'odt',
          'epub': 'epub', 'rst': 'rst', 'org': 'org', 'txt': 'plain',
          'json': 'json', 'native': 'native', 'xml': 'docbook', 'rtf': 'rtf',
          'adoc': 'asciidoc', 'ipynb': 'ipynb', 'typ': 'typst', 'pptx': 'pptx',
          'dj': 'djot', 'man': 'man', '1': 'man', 'ms': 'ms', 'texi': 'texinfo',
          'icml': 'icml', 'opml': 'opml', 'bib': 'bibtex', 'wiki': 'mediawiki'
        };
        return formatByOutputExtension[ext] || 'html';
      }
      return 'html';
    },

    get outputFilenamePlaceholder() {
      if (this.outputFormat !== 'auto') {
        const outExt = this.extensionByFormat[this.outputFormat] || this.outputFormat;
        if (this.fileOrder.length > 0) {
          const baseName = this.fileOrder[0].replace(/\.[^.]+$/, '');
          return `${baseName}.${outExt}`;
        }
        return `output.${outExt}`;
      }
      return '(preview)';
    },

    get isHtmlFormat() { return this.htmlFormats.includes(this.effectiveOutputFormat); },
    get isMarkdownFormat() { return this.markdownFormats.includes(this.effectiveOutputFormat); },
    get isLatexLikeFormat() { return this.latexCitationFormats.includes(this.effectiveOutputFormat); },
    get supportsCaptionPosition() { return this.captionPositionFormats.includes(this.effectiveOutputFormat); },
    get supportsAscii() { return this.asciiFormats.includes(this.effectiveOutputFormat); },
    get supportsTopLevelDivision() { return this.topLevelDivisionFormats.includes(this.effectiveOutputFormat); },
    get supportsListOf() { return this.listOfFormats.includes(this.effectiveOutputFormat); },
    get isBinaryOutput() { return this.binaryFormats.includes(this.effectiveOutputFormat); },
    get filteredMessages() {
      if (this.verbosity === 'error') {
        return this.messages.filter(m => m.type === 'error');
      } else if (this.verbosity === 'warning') {
        return this.messages.filter(m => m.type === 'error' || m.type === 'warning');
      }
      return this.messages;
    },

    get suggestedVariables() {
      const fmt = this.effectiveOutputFormat;
      const categories = this.formatVariableCategories[fmt] || ['all'];
      const vars = new Set();
      for (const cat of categories) {
        const catVars = this.templateVariablesByCategory[cat] || [];
        for (const v of catVars) vars.add(v);
      }
      return Array.from(vars).sort();
    },

    get effectiveInputFormat() {
      let inFmt = this.inputFormat;
      if (inFmt === 'auto' && this.fileOrder.length > 0) {
        const ext = this.fileOrder[0].split('.').pop().toLowerCase();
        inFmt = this.formatByExtension[ext] || '';
      }
      return inFmt;
    },

    get showTrackChangesTab() {
      return this.effectiveInputFormat === 'docx';
    },

    get showStripComments() {
      const fmt = this.effectiveInputFormat;
      return fmt === 'auto' || this.markdownFormats.includes(fmt) || fmt === 'textile';
    },

    get showDefaultImageExtension() {
      const fmt = this.effectiveInputFormat;
      return fmt === 'auto' || this.markdownFormats.includes(fmt) || fmt === 'latex';
    },

    get showAbbreviations() {
      const fmt = this.effectiveInputFormat;
      return fmt === 'auto' || fmt === 'markdown';
    },

    get showExtensionsTab() {
      return (this.inputFormat !== 'auto' && this.inputExtensionsList.length > 0) ||
        (this.outputFormat !== 'auto' && this.outputExtensionsList.length > 0);
    },

    get showFormatTab() {
      const fmt = this.effectiveOutputFormat;
      return this.showStripComments || this.showDefaultImageExtension || this.showAbbreviations ||
        this.isHtmlFormat || this.isMarkdownFormat || fmt === 'rst' ||
        ['latex', 'beamer'].includes(fmt) || ['typst', 'pdf'].includes(fmt) ||
        this.supportsCaptionPosition || this.supportsAscii || this.supportsTopLevelDivision ||
        this.supportsListOf || this.docFormats.includes(fmt);
    },

    get formatTabName() {
      return 'Format-specific';
    },

    get isEpubFormat() { return ['epub', 'epub2', 'epub3'].includes(this.effectiveOutputFormat); },
    get showChunkedTab() { return this.effectiveOutputFormat === 'chunkedhtml'; },
    get showIpynbTab() { return this.effectiveOutputFormat === 'ipynb'; },
    get showSlidesTab() { return this.slideFormats.includes(this.effectiveOutputFormat); },
    get showReferenceDoc() { return this.docFormats.includes(this.effectiveOutputFormat); },

    // Methods
    getFormatLabel(fmt, direction) {
      const name = this.formatNames[fmt] || fmt;
      return `${fmt} (${name})`;
    },

    init() {
      // Register callback to populate formats when pandoc loads
      onFormatsLoaded = (inputFmts, outputFmts) => {
        this.inputFormats = inputFmts;
        this.outputFormats = outputFmts;
        this.formatsLoaded = true;
      };
      console.log('Pandoc converter initialized (pandoc.wasm loading in background)');
    },

    async ensurePandocLoaded() {
      if (this.pandocReady) return;
      this.waitingForPandoc = true;
      try {
        await loadPandoc();
        this.pandocReady = true;
      } finally {
        this.waitingForPandoc = false;
      }
    },

    async loadExample() {
      const zipPath = this.selectedExample;
      if (!zipPath) return;

      try {
        // Fetch the example first
        const response = await fetch(zipPath + "?sha1=SHA1_EXAMPLES");
        const arrayBuffer = await response.arrayBuffer();

        // Ensure pandoc is loaded before processing (which auto-converts)
        await this.ensurePandocLoaded();

        await this.loadExampleFromBuffer(arrayBuffer, zipPath.split('/').pop());
      } catch (err) {
        this.messages.push({ type: 'error', text: `Failed to load example: ${err.message}` });
      }
    },

    async uploadExample(e) {
      const file = e.target.files[0];
      if (!file) return;

      try {
        const arrayBuffer = await file.arrayBuffer();
        e.target.value = '';

        // Ensure pandoc is loaded before processing (which auto-converts)
        await this.ensurePandocLoaded();

        await this.loadExampleFromBuffer(arrayBuffer, file.name);
      } catch (err) {
        this.showOutput = true;
        this.messages.push({ type: 'error', text: `Failed to load example: ${err.message}` });
      }
    },

    async loadExampleFromBuffer(arrayBuffer, filename) {
      const zipData = fflate.unzipSync(new Uint8Array(arrayBuffer));

      // Clear existing state
      this.files = {};
      this.fileOrder = [];
      this.auxFiles = {};
      this.textInput = '';
      this.templateFile = null;
      this.referenceDoc = null;
      this.bibFile = null;
      this.cslFile = null;
      this.cslStyleName = '';
      this.cssFiles = [];
      this.headerFiles = [];
      this.beforeBodyFiles = [];
      this.afterBodyFiles = [];
      this.highlightThemeFile = null;
      this.syntaxDefinitions = [];
      this.abbrevFile = null;
      this.abbreviationsFile = null;
      this.metadataFile = null;
      this.epubCoverImage = null;
      this.epubMetadataFile = null;
      this.epubFonts = [];
      this.luaFilters = [];

      let optionsJson = null;
      let stdinContent = null;
      const extractedFiles = {};

      // First pass: extract all files
      for (const [name, data] of Object.entries(zipData)) {
        if (name.endsWith('/') || data.length === 0) continue;
        const fname = name.split('/').pop();

        if (fname === 'options.json') {
          const txt = fflate.strFromU8(data);
          optionsJson = JSON.parse(txt);
        } else if (fname === 'stdin') {
          stdinContent = fflate.strFromU8(data);
        } else {
          extractedFiles[fname] = new File([data], fname);
        }
      }

      // Helper to get file and remove from extractedFiles
      const takeFile = (name) => {
        if (name && extractedFiles[name]) {
          const f = extractedFiles[name];
          delete extractedFiles[name];
          return f;
        }
        return null;
      };

      // Helper for arrays of filenames
      const takeFiles = (names) => {
        if (!names) return [];
        const arr = Array.isArray(names) ? names : [names];
        return arr.map(takeFile).filter(f => f);
      };

      // Route files based on options.json
      if (optionsJson) {
        // Template
        this.templateFile = takeFile(optionsJson.template);

        // Reference doc
        this.referenceDoc = takeFile(optionsJson['reference-doc']);

        // Bibliography
        const bibFiles = takeFiles(optionsJson.bibliography);
        if (bibFiles.length > 0) this.bibFile = bibFiles[0];

        // CSL style
        this.cslFile = takeFile(optionsJson.csl);

        // CSS files
        this.cssFiles = takeFiles(optionsJson.css);

        // Include files
        this.headerFiles = takeFiles(optionsJson['include-in-header']);
        this.beforeBodyFiles = takeFiles(optionsJson['include-before-body']);
        this.afterBodyFiles = takeFiles(optionsJson['include-after-body']);

        // Highlight theme (if it's a file)
        const hlStyle = optionsJson['highlight-style'];
        if (hlStyle && extractedFiles[hlStyle]) {
          this.highlightThemeFile = takeFile(hlStyle);
          this.opts.highlightStyle = 'custom';
        } else if (hlStyle) {
          this.opts.highlightStyle = hlStyle;
        }

        // Syntax definitions
        this.syntaxDefinitions = takeFiles(optionsJson['syntax-definition']);

        // Abbreviations
        this.abbrevFile = takeFile(optionsJson.abbreviations);

        // Metadata file
        this.metadataFile = takeFile(optionsJson['metadata-file']);

        // EPUB cover image
        this.epubCoverImage = takeFile(optionsJson['epub-cover-image']);

        // EPUB fonts
        this.epubFonts = takeFiles(optionsJson['epub-fonts']);

        // Lua filters
        this.luaFilters = takeFiles(optionsJson.filters);

        // Input files go to main files area
        const inputFileNames = optionsJson['input-files'];
        if (inputFileNames) {
          const inputArr = Array.isArray(inputFileNames) ? inputFileNames : [inputFileNames];
          for (const name of inputArr) {
            const f = takeFile(name);
            if (f) {
              this.files[name] = f;
              this.fileOrder.push(name);
            }
          }
        }

        // Set input format
        if (optionsJson.from) {
          this.inputFormat = optionsJson.from.split(/[+-]/)[0];
        }
        // Set output format
        if (optionsJson.to) {
          this.outputFormat = optionsJson.to.split(/[+-]/)[0];
        }
        // Set standalone
        if (optionsJson.standalone !== undefined) {
          this.opts.standalone = optionsJson.standalone;
        }
        // Other options
        if (optionsJson['wrap']) this.opts.wrap = optionsJson['wrap'];
        if (optionsJson['table-of-contents']) this.opts.toc = true;
        if (optionsJson['number-sections']) this.opts.numberSections = true;
        if (optionsJson.citeproc) this.opts.citationMethod = 'citeproc';
        if (optionsJson['embed-resources']) this.opts.selfContained = true;
        if (optionsJson['mathml']) this.opts.mathMethod = 'mathml';
      }

      // Remaining files go to auxiliary files
      for (const [name, file] of Object.entries(extractedFiles)) {
        this.auxFiles[name] = file;
      }

      // Handle stdin content
      if (stdinContent) {
        this.inputMode = 'text';
        this.textInput = stdinContent;
      } else if (this.fileOrder.length > 0) {
        this.inputMode = 'file';
      }

      // Auto-convert after loading example
      await this.convert();

      // Update extensions UI after conversion completes
      this.onInputFormatChange();
      this.onOutputFormatChange();
    },

    async downloadAsExample() {
      const zipFiles = {};

      // Build options for the example
      const opts = {};
      if (this.inputFormat !== 'auto') opts.from = this.inputFormat;
      if (this.outputFormat !== 'auto') opts.to = this.outputFormat;
      if (this.opts.standalone) opts.standalone = true;
      if (this.opts.toc) opts['table-of-contents'] = true;
      if (this.opts.numberSections) opts['number-sections'] = true;
      if (this.opts.fileScope) opts['file-scope'] = true;
      if (this.opts.citationMethod === 'citeproc') opts.citeproc = true;
      if (this.opts.mathMethod === 'mathml') opts.mathml = true;
      if (this.opts.selfContained) opts['embed-resources'] = true;
      if (this.opts.highlightStyle && this.opts.highlightStyle !== 'custom') {
        opts['highlight-style'] = this.opts.highlightStyle;
      }

      // Helper to add a file to the zip
      const addFile = async (file, name) => {
        const arrayBuffer = await file.arrayBuffer();
        zipFiles[name] = new Uint8Array(arrayBuffer);
      };

      // Input files or stdin
      if (this.inputMode === 'text' && this.textInput.trim()) {
        zipFiles['stdin'] = fflate.strToU8(this.textInput);
      } else if (this.inputMode === 'file' && this.fileOrder.length > 0) {
        opts['input-files'] = this.fileOrder;
        for (const name of this.fileOrder) {
          if (this.files[name]) await addFile(this.files[name], name);
        }
      }

      // Auxiliary files
      for (const [name, file] of Object.entries(this.auxFiles)) {
        await addFile(file, name);
      }

      // Resource files
      if (this.templateFile) {
        opts.template = this.templateFile.name;
        await addFile(this.templateFile, this.templateFile.name);
      }
      if (this.referenceDoc) {
        opts['reference-doc'] = this.referenceDoc.name;
        await addFile(this.referenceDoc, this.referenceDoc.name);
      }
      if (this.bibFile) {
        opts.bibliography = this.bibFile.name;
        await addFile(this.bibFile, this.bibFile.name);
      }
      if (this.cslFile) {
        opts.csl = this.cslFile.name;
        await addFile(this.cslFile, this.cslFile.name);
      }
      if (this.abbrevFile) {
        opts['citation-abbreviations'] = this.abbrevFile.name;
        await addFile(this.abbrevFile, this.abbrevFile.name);
      }
      if (this.abbreviationsFile) {
        opts.abbreviations = this.abbreviationsFile.name;
        await addFile(this.abbreviationsFile, this.abbreviationsFile.name);
      }
      if (this.metadataFile) {
        opts['metadata-file'] = this.metadataFile.name;
        await addFile(this.metadataFile, this.metadataFile.name);
      }
      if (this.highlightThemeFile) {
        opts['highlight-style'] = this.highlightThemeFile.name;
        await addFile(this.highlightThemeFile, this.highlightThemeFile.name);
      }
      if (this.cssFiles.length > 0) {
        opts.css = this.cssFiles.map(f => f.name);
        for (const file of this.cssFiles) await addFile(file, file.name);
      }
      if (this.syntaxDefinitions.length > 0) {
        opts['syntax-definition'] = this.syntaxDefinitions.map(f => f.name);
        for (const file of this.syntaxDefinitions) await addFile(file, file.name);
      }
      if (this.headerFiles.length > 0) {
        opts['include-in-header'] = this.headerFiles.map(f => f.name);
        for (const file of this.headerFiles) await addFile(file, file.name);
      }
      if (this.beforeBodyFiles.length > 0) {
        opts['include-before-body'] = this.beforeBodyFiles.map(f => f.name);
        for (const file of this.beforeBodyFiles) await addFile(file, file.name);
      }
      if (this.afterBodyFiles.length > 0) {
        opts['include-after-body'] = this.afterBodyFiles.map(f => f.name);
        for (const file of this.afterBodyFiles) await addFile(file, file.name);
      }
      if (this.epubCoverImage) {
        opts['epub-cover-image'] = this.epubCoverImage.name;
        await addFile(this.epubCoverImage, this.epubCoverImage.name);
      }
      if (this.epubFonts.length > 0) {
        opts['epub-fonts'] = this.epubFonts.map(f => f.name);
        for (const file of this.epubFonts) await addFile(file, file.name);
      }
      if (this.luaFilters.length > 0) {
        opts.filters = this.luaFilters.map(f => f.name);
        for (const file of this.luaFilters) await addFile(file, file.name);
      }

      // Add options.json
      zipFiles['options.json'] = fflate.strToU8(JSON.stringify(opts, null, 2));

      // Create zip and download
      const zipData = fflate.zipSync(zipFiles);
      const blob = new Blob([zipData], { type: 'application/zip' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = 'example.zip';
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    },

    formatFileSize(bytes) {
      if (bytes < 1024) return bytes + ' B';
      if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
      return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
    },

    downloadFile(file, name) {
      const url = URL.createObjectURL(file);
      const a = document.createElement('a');
      a.href = url;
      a.download = name;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    },

    handleDrop(e) {
      this.dragOver = false;
      this.handleFiles(Array.from(e.dataTransfer.files));
    },

    handleFileInput(e) {
      this.handleFiles(Array.from(e.target.files));
      e.target.value = '';
    },

    handleFiles(fileList) {
      fileList.forEach(file => {
        if (!this.files[file.name]) {
          this.fileOrder.push(file.name);
        }
        this.files[file.name] = file;
      });
      this.updateExtensions();
      this.hasChanges = true;
    },

    removeFile(name) {
      delete this.files[name];
      this.fileOrder = this.fileOrder.filter(n => n !== name);
      this.hasChanges = true;
    },

    onFileDragStart(idx, e) {
      this.fileDraggingIdx = idx;
      e.dataTransfer.effectAllowed = 'move';
    },

    onFileDragOver(idx) {
      this.fileDragOverIdx = idx;
    },

    onFileDrop(idx) {
      this.fileDragOverIdx = -1;
      if (this.fileDraggingIdx !== -1 && this.fileDraggingIdx !== idx) {
        const draggedName = this.fileOrder[this.fileDraggingIdx];
        this.fileOrder.splice(this.fileDraggingIdx, 1);
        this.fileOrder.splice(idx, 0, draggedName);
        this.hasChanges = true;
      }
    },

    handleAuxFiles(e) {
      Array.from(e.target.files).forEach(file => { this.auxFiles[file.name] = file; });
      e.target.value = '';
      this.hasChanges = true;
    },

    handleAuxDir(e) {
      Array.from(e.target.files).forEach(file => {
        const path = file.webkitRelativePath || file.name;
        this.auxFiles[path] = file;
      });
      e.target.value = '';
      this.hasChanges = true;
    },

    removeAuxFile(name) { delete this.auxFiles[name]; this.hasChanges = true; },

    // Resource file handlers
    handleBibFile(e) { if (e.target.files[0]) { this.bibFile = e.target.files[0]; this.hasChanges = true; } },
    handleCslFile(e) { if (e.target.files[0]) { this.cslFile = e.target.files[0]; this.cslStyleName = ''; this.hasChanges = true; } },
    handleAbbrevFile(e) { if (e.target.files[0]) { this.abbrevFile = e.target.files[0]; this.hasChanges = true; } },
    handleAbbreviationsFile(e) { if (e.target.files[0]) { this.abbreviationsFile = e.target.files[0]; this.hasChanges = true; } },
    handleHighlightTheme(e) { if (e.target.files[0]) { this.highlightThemeFile = e.target.files[0]; this.hasChanges = true; } },
    handleTemplateFile(e) { if (e.target.files[0]) { this.templateFile = e.target.files[0]; this.hasChanges = true; } },
    handleReferenceDoc(e) { if (e.target.files[0]) { this.referenceDoc = e.target.files[0]; this.hasChanges = true; } },
    handleMetadataFile(e) { if (e.target.files[0]) { this.metadataFile = e.target.files[0]; this.hasChanges = true; } },
    handleEpubCover(e) { if (e.target.files[0]) { this.epubCoverImage = e.target.files[0]; this.hasChanges = true; } },
    handleEpubMetadata(e) { if (e.target.files[0]) { this.epubMetadataFile = e.target.files[0]; this.hasChanges = true; } },

    // Generic file list handlers
    addFilesToList(e, list) {
      Array.from(e.target.files).forEach(file => {
        if (!list.find(f => f.name === file.name)) list.push(file);
      });
      e.target.value = '';
      this.hasChanges = true;
    },
    removeFileFromList(name, list) {
      const idx = list.findIndex(f => f.name === name);
      if (idx !== -1) list.splice(idx, 1);
      this.hasChanges = true;
    },

    handleCssFiles(e) { this.addFilesToList(e, this.cssFiles); },
    removeCssFile(name) { this.removeFileFromList(name, this.cssFiles); },
    handleSyntaxDefs(e) { this.addFilesToList(e, this.syntaxDefinitions); },
    removeSyntaxDef(name) { this.removeFileFromList(name, this.syntaxDefinitions); },
    handleEpubFonts(e) { this.addFilesToList(e, this.epubFonts); },
    removeEpubFont(name) { this.removeFileFromList(name, this.epubFonts); },
    handleHeaderFiles(e) { this.addFilesToList(e, this.headerFiles); },
    removeHeaderFile(name) { this.removeFileFromList(name, this.headerFiles); },
    handleBeforeBodyFiles(e) { this.addFilesToList(e, this.beforeBodyFiles); },
    removeBeforeBodyFile(name) { this.removeFileFromList(name, this.beforeBodyFiles); },
    handleAfterBodyFiles(e) { this.addFilesToList(e, this.afterBodyFiles); },
    removeAfterBodyFile(name) { this.removeFileFromList(name, this.afterBodyFiles); },
    handleLuaFilters(e) { this.addFilesToList(e, this.luaFilters); },
    removeLuaFilter(name) { this.removeFileFromList(name, this.luaFilters); },

    onFilterDragStart(idx, e) {
      this.filterDraggingIdx = idx;
      e.dataTransfer.effectAllowed = 'move';
    },

    onFilterDragOver(idx) {
      this.filterDragOverIdx = idx;
    },

    onFilterDrop(idx) {
      this.filterDragOverIdx = -1;
      if (this.filterDraggingIdx !== -1 && this.filterDraggingIdx !== idx) {
        const draggedFilter = this.luaFilters[this.filterDraggingIdx];
        this.luaFilters.splice(this.filterDraggingIdx, 1);
        this.luaFilters.splice(idx, 0, draggedFilter);
        this.hasChanges = true;
      }
    },

    // Metadata auto-add row
    onMetadataInput() {
      const last = this.customMetadata[this.customMetadata.length - 1];
      if (last && (last.key || last.value)) {
        this.customMetadata.push({ key: '', value: '' });
      }
      this.hasChanges = true;
    },
    removeMetadata(idx) {
      this.customMetadata.splice(idx, 1);
      if (this.customMetadata.length === 0) {
        this.customMetadata.push({ key: '', value: '' });
      }
      this.hasChanges = true;
    },

    // Variables auto-add row
    onVariableInput() {
      const last = this.customVariables[this.customVariables.length - 1];
      if (last && (last.key || last.value)) {
        this.customVariables.push({ key: '', value: '' });
      }
      this.hasChanges = true;
    },
    removeVariable(idx) {
      this.customVariables.splice(idx, 1);
      if (this.customVariables.length === 0) {
        this.customVariables.push({ key: '', value: '' });
      }
      this.hasChanges = true;
    },

    // CSL styles
    async loadCslStyles() {
      if (this.cslStylesLoaded) return;
      try {
        const response = await fetch('https://data.jsdelivr.com/v1/package/gh/citation-style-language/styles@master/flat');
        if (!response.ok) throw new Error('Failed to fetch styles list');
        const data = await response.json();
        this.cslStylesList = data.files
          .map(f => f.name)
          .filter(name => name.endsWith('.csl') && name.lastIndexOf('/') === 0)
          .map(name => name.slice(1, -4))
          .sort();
        this.cslStylesLoaded = true;
      } catch (err) {
        console.error('Failed to load CSL styles list:', err);
        this.cslStylesList = ['apa', 'chicago-author-date', 'ieee', 'modern-language-association', 'vancouver', 'harvard-cite-them-right', 'nature', 'science'];
      }
    },

    async fetchCslStyle() {
      if (!this.cslStyleName.trim()) {
        alert('Please enter or select a style name');
        return;
      }
      try {
        const url = `https://cdn.jsdelivr.net/gh/citation-style-language/styles@master/${this.cslStyleName}.csl`;
        const response = await fetch(url);
        if (!response.ok) throw new Error(`Style "${this.cslStyleName}" not found`);
        const cslContent = await response.text();
        const blob = new Blob([cslContent], { type: 'application/xml' });
        this.cslFile = new File([blob], `${this.cslStyleName}.csl`, { type: 'application/xml' });
        this.hasChanges = true;
      } catch (err) {
        alert(`Failed to fetch style: ${err.message}`);
      }
    },

    // Extensions
    async updateExtensions() {
      this.inputExtensions = {};
      this.outputExtensions = {};
      this.inputExtensionsList = [];
      this.outputExtensionsList = [];

      if (this.inputFormat !== 'auto') {
        try {
          const extData = await window.pandocModule.query({ query: "extensions-for-format", format: this.inputFormat });
          this.inputExtensionsList = Object.entries(extData)
            .map(([name, defaultOn]) => ({ name, defaultOn }))
            .sort((a, b) => a.name.localeCompare(b.name));
        } catch (e) { console.warn('Could not get input extensions:', e); }
      }

      const outFmt = this.outputFormat === 'pdf' ? 'typst' : this.outputFormat;
      if (outFmt !== 'auto') {
        try {
          const extData = await window.pandocModule.query({ query: "extensions-for-format", format: outFmt});
          this.outputExtensionsList = Object.entries(extData)
            .map(([name, defaultOn]) => ({ name, defaultOn }))
            .sort((a, b) => a.name.localeCompare(b.name));
        } catch (e) { console.warn('Could not get output extensions:', e); }
      }
    },

    getExtensionChecked(direction, ext) {
      const extState = direction === 'input' ? this.inputExtensions : this.outputExtensions;
      if (extState[ext.name] !== undefined) return extState[ext.name];
      return ext.defaultOn;
    },

    toggleExtension(direction, ext, e) {
      const extState = direction === 'input' ? this.inputExtensions : this.outputExtensions;
      if (e.target.checked) {
        extState[ext.name] = true;
      } else {
        if (ext.defaultOn) {
          extState[ext.name] = false;
        } else {
          delete extState[ext.name];
        }
      }
    },

    onInputFormatChange() { this.updateExtensions(); },
    onOutputFormatChange() { this.updateExtensions(); },

    buildFormatWithExtensions(baseFormat, extensions) {
      if (!baseFormat || baseFormat === 'auto') return baseFormat;
      let formatStr = baseFormat;
      for (const [ext, enabled] of Object.entries(extensions)) {
        if (enabled === true) formatStr += `+${ext}`;
        else if (enabled === false) formatStr += `-${ext}`;
      }
      return formatStr;
    },

    buildOptions() {
      const opts = {};

      // Input format
      if (this.inputFormat !== 'auto') {
        opts.from = this.buildFormatWithExtensions(this.inputFormat, this.inputExtensions);
      } else if (this.inputMode === 'file' && this.fileOrder.length > 0) {
        const ext = this.fileOrder[0].split('.').pop().toLowerCase();
        if (this.formatByExtension[ext]) {
          opts.from = this.buildFormatWithExtensions(this.formatByExtension[ext], this.inputExtensions);
        }
      }

      // Output format
      if (this.outputFormat !== 'auto') {
        opts.to = this.buildFormatWithExtensions(this.outputFormat, this.outputExtensions);
      }

      // Output file
      let finalOutFile = this.outputFilename.trim() || this.outputFilenamePlaceholder;
      if (!finalOutFile || finalOutFile === '(preview)') {
        const effectiveFmt = this.effectiveOutputFormat || 'html';
        const defaultExt = this.extensionByFormat[effectiveFmt] || 'html';
        finalOutFile = `output.${defaultExt}`;
      }
      opts['output-file'] = finalOutFile;
      this.outputFilenameActual = finalOutFile;

      // Input files
      if (this.inputMode === 'file') {
        opts['input-files'] = this.fileOrder;
      }

      // General options
      if (this.opts.standalone) opts.standalone = true;
      if (this.opts.toc) {
        opts['table-of-contents'] = true;
        opts['toc-depth'] = parseInt(this.opts.tocDepth);
      }
      if (this.opts.numberSections) opts['number-sections'] = true;
      if (this.opts.fileScope) opts['file-scope'] = true;
      const shiftHeading = parseInt(this.opts.shiftHeading);
      if (shiftHeading !== 0) opts['shift-heading-level-by'] = shiftHeading;
      if (this.opts.preserveTabs) opts['preserve-tabs'] = true;
      const tabStop = parseInt(this.opts.tabStop);
      if (tabStop !== 4) opts['tab-stop'] = tabStop;
      if (this.opts.eol) opts.eol = this.opts.eol;
      if (this.opts.dpi) opts.dpi = parseInt(this.opts.dpi);
      if (this.opts.numberOffset.trim()) {
        opts['number-offset'] = this.opts.numberOffset.trim().split(',').map(n => parseInt(n.trim()) || 0);
      }
      if (this.opts.resourcePath.trim()) opts['resource-path'] = this.opts.resourcePath.trim();
      if (this.opts.stripComments) opts['strip-comments'] = true;
      if (this.opts.defaultImageExtension.trim()) opts['default-image-extension'] = this.opts.defaultImageExtension.trim();
      if (this.abbreviationsFile) opts.abbreviations = this.abbreviationsFile.name;

      // Track changes
      if (this.opts.trackChanges) opts['track-changes'] = this.opts.trackChanges;

      // Citations
      if (this.opts.citationMethod === 'citeproc') opts.citeproc = true;
      else if (this.opts.citationMethod === 'natbib') opts.natbib = true;
      else if (this.opts.citationMethod === 'biblatex') opts.biblatex = true;
      if (this.bibFile) opts.bibliography = this.bibFile.name;
      if (this.opts.citationMethod === 'citeproc') {
        if (this.cslFile) opts.csl = this.cslFile.name;
        if (this.abbrevFile) opts['citation-abbreviations'] = this.abbrevFile.name;
      }

      // Math
      if (this.opts.mathMethod) opts['html-math-method'] = this.opts.mathMethod;

      // Code highlighting
      if (this.opts.highlightStyle === 'custom' && this.highlightThemeFile) {
        opts['highlight-style'] = this.highlightThemeFile.name;
      } else if (this.opts.highlightStyle && this.opts.highlightStyle !== 'custom') {
        opts['highlight-style'] = this.opts.highlightStyle;
      }
      if (this.syntaxDefinitions.length > 0) {
        opts['syntax-definition'] = this.syntaxDefinitions.map(f => f.name);
      }
      if (this.opts.indentedCodeClasses.trim()) {
        opts['indented-code-classes'] = this.opts.indentedCodeClasses.trim();
      }

      const baseOutFmt = this.effectiveOutputFormat;

      // HTML options
      if (this.htmlFormats.includes(baseOutFmt)) {
        if (this.opts.selfContained) opts['embed-resources'] = true;
        if (this.opts.htmlQTags) opts['html-q-tags'] = true;
        if (this.opts.sectionDivs) opts['section-divs'] = true;
        if (this.opts.titlePrefix.trim()) opts['title-prefix'] = this.opts.titlePrefix.trim();
        if (this.opts.emailObfuscation) opts['email-obfuscation'] = this.opts.emailObfuscation;
        if (this.opts.idPrefix.trim()) opts['id-prefix'] = this.opts.idPrefix.trim();
        if (this.cssFiles.length > 0) opts.css = this.cssFiles.map(f => f.name);
      }

      // Markdown options
      if (this.markdownFormats.includes(baseOutFmt)) {
        if (this.opts.referenceLinks) opts['reference-links'] = true;
        if (this.opts.referenceLocation) opts['reference-location'] = this.opts.referenceLocation;
        if (this.opts.markdownHeadings) opts['markdown-headings'] = this.opts.markdownHeadings;
      }

      // RST options
      if (baseOutFmt === 'rst') {
        if (this.opts.referenceLinksRst) opts['reference-links'] = true;
        if (this.opts.listTables) opts['list-tables'] = true;
      }

      // Caption position
      if (this.captionPositionFormats.includes(baseOutFmt)) {
        if (this.opts.figureCaptionPosition) opts['figure-caption-position'] = this.opts.figureCaptionPosition;
        if (this.opts.tableCaptionPosition) opts['table-caption-position'] = this.opts.tableCaptionPosition;
      }

      // ASCII
      if (this.asciiFormats.includes(baseOutFmt)) {
        const asciiChecked = this.htmlFormats.includes(baseOutFmt) ? this.opts.asciiHtml : this.opts.ascii;
        if (asciiChecked) opts.ascii = true;
      }

      // List of figures/tables
      if (this.listOfFormats.includes(baseOutFmt)) {
        if (this.opts.lof) opts['list-of-figures'] = true;
        if (this.opts.lot) opts['list-of-tables'] = true;
      }

      // Top-level division
      if (this.topLevelDivisionFormats.includes(baseOutFmt) && this.opts.topLevelDivision) {
        opts['top-level-division'] = this.opts.topLevelDivision;
      }

      // EPUB options
      if (['epub', 'epub2', 'epub3'].includes(baseOutFmt)) {
        if (this.epubCoverImage) opts['epub-cover-image'] = this.epubCoverImage.name;
        if (this.epubMetadataFile) opts['epub-metadata'] = this.epubMetadataFile.name;
        if (this.epubFonts.length > 0) opts['epub-fonts'] = this.epubFonts.map(f => f.name);
        if (!this.opts.epubTitlePage) opts['epub-title-page'] = false;
        if (this.opts.epubSubdirectory.trim()) opts['epub-subdirectory'] = this.opts.epubSubdirectory.trim();
      }

      // ODT options
      if (baseOutFmt === 'odt') {
        if (this.opts.linkImages) opts['link-images'] = true;
      }

      // Template
      if (this.templateFile) opts.template = this.templateFile.name;

      // Include files
      if (this.headerFiles.length > 0) opts['include-in-header'] = this.headerFiles.map(f => f.name);
      if (this.beforeBodyFiles.length > 0) opts['include-before-body'] = this.beforeBodyFiles.map(f => f.name);
      if (this.afterBodyFiles.length > 0) opts['include-after-body'] = this.afterBodyFiles.map(f => f.name);

      // Variables
      const variables = {};
      this.customVariables.forEach(v => {
        if (v.key.trim() && v.value.trim()) variables[v.key.trim()] = v.value.trim();
      });
      if (Object.keys(variables).length > 0) opts.variables = variables;

      // Chunked HTML
      if (baseOutFmt === 'chunkedhtml') {
        if (this.opts.splitLevel) opts['split-level'] = parseInt(this.opts.splitLevel);
        if (this.opts.chunkTemplate.trim()) opts['chunk-template'] = this.opts.chunkTemplate.trim();
      }

      // Notebook
      if (baseOutFmt === 'ipynb' && this.opts.ipynbOutput) {
        opts['ipynb-output'] = this.opts.ipynbOutput;
      }

      // Reference doc
      if (this.docFormats.includes(baseOutFmt) && this.referenceDoc) {
        opts['reference-doc'] = this.referenceDoc.name;
      }

      // Slides
      if (this.slideFormats.includes(baseOutFmt)) {
        if (this.opts.slideLevel) opts['slide-level'] = parseInt(this.opts.slideLevel);
        if (this.opts.incremental) opts.incremental = true;
      }

      // Metadata file
      if (this.metadataFile) opts['metadata-file'] = this.metadataFile.name;

      // Metadata
      const metadata = {};
      this.customMetadata.forEach(m => {
        if (m.key.trim() && m.value.trim()) {
          const key = m.key.trim();
          const value = m.value.trim();
          // author should be an array
          if (key === 'author') {
            metadata[key] = [value];
          } else {
            metadata[key] = value;
          }
        }
      });
      if (Object.keys(metadata).length > 0) opts.metadata = metadata;

      // Wrapping
      if (this.opts.wrap !== 'auto') opts.wrap = this.opts.wrap;
      if (this.opts.columns.trim()) opts.columns = parseInt(this.opts.columns);

      // Extract media
      if (this.opts.extractMedia) opts['extract-media'] = 'media.zip';

      // Lua filters
      if (this.luaFilters.length > 0) {
        opts.filters = this.luaFilters.map(f => f.name);
      }

      return opts;
    },

    async convert() {
      // Ensure pandoc is loaded before converting
      await this.ensurePandocLoaded();

      this.isConverting = true;
      this.showOutput = false;
      this.messages = [];
      this.output = null;
      this.outputPreview = '';
      this.mediaZip = null;

      try {
        const options = this.buildOptions();

        // Build files object
        const files = { ...this.files, ...this.auxFiles };
        if (this.bibFile) files[this.bibFile.name] = this.bibFile;
        if (this.cslFile) files[this.cslFile.name] = this.cslFile;
        if (this.abbrevFile) files[this.abbrevFile.name] = this.abbrevFile;
        if (this.abbreviationsFile) files[this.abbreviationsFile.name] = this.abbreviationsFile;
        this.cssFiles.forEach(file => { files[file.name] = file; });
        if (this.highlightThemeFile) files[this.highlightThemeFile.name] = this.highlightThemeFile;
        this.syntaxDefinitions.forEach(file => { files[file.name] = file; });
        if (this.templateFile) files[this.templateFile.name] = this.templateFile;
        if (this.referenceDoc) files[this.referenceDoc.name] = this.referenceDoc;
        if (this.metadataFile) files[this.metadataFile.name] = this.metadataFile;
        if (this.epubCoverImage) files[this.epubCoverImage.name] = this.epubCoverImage;
        if (this.epubMetadataFile) files[this.epubMetadataFile.name] = this.epubMetadataFile;
        this.epubFonts.forEach(font => { files[font.name] = font; });
        this.headerFiles.forEach(file => { files[file.name] = file; });
        this.beforeBodyFiles.forEach(file => { files[file.name] = file; });
        this.afterBodyFiles.forEach(file => { files[file.name] = file; });
        this.luaFilters.forEach(file => { files[file.name] = file; });

        const stdin = this.inputMode === 'text' ? this.textInput : null;
        const isPdfTypst = this.outputFormat === 'pdf';

        if (isPdfTypst) {
          await window.loadTypst();
          const typstOptions = { ...options };
          typstOptions.to = this.buildFormatWithExtensions('typst', this.outputExtensions);
          typstOptions.standalone = true;
          delete typstOptions['output-file'];

          const typstResult = await window.pandocModule.convert(typstOptions, stdin, files);
          if (typstResult.stderr && typstResult.stderr.includes('ERROR')) {
            throw new Error(typstResult.stderr);
          }

          const typstContent = typstResult.stdout;
          let pdfData;
          try {
            $typst.resetShadow();
            for (const [path, file] of Object.entries(files)) {
              const arrayBuffer = await file.arrayBuffer();
              const uint8Array = new Uint8Array(arrayBuffer);
              $typst.mapShadow('/' + path, uint8Array);
              if (!path.startsWith('/')) $typst.mapShadow(path, uint8Array);
            }
            const mainTypstPath = '/main.typ';
            const typstBytes = new TextEncoder().encode(typstContent);
            $typst.mapShadow(mainTypstPath, typstBytes);
            pdfData = await $typst.pdf({ mainFilePath: mainTypstPath });
          } catch (typstError) {
            const errorStr = String(typstError);
            const messageMatch = errorStr.match(/message:\s*"([^"]+)"/);
            let errorMsg = messageMatch ? messageMatch[1] : errorStr;
            throw new Error(`Typst: ${errorMsg}`);
          }

          if (!pdfData || pdfData.length === 0) {
            throw new Error('Typst produced empty PDF output');
          }

          const pdfBlob = new Blob([pdfData], { type: 'application/pdf' });
          this.output = pdfBlob;
          this.displayResults(typstResult, options, files);
        } else {
          const result = await window.pandocModule.convert(options, stdin, files);
          this.displayResults(result, options, files);
        }
      } catch (err) {
        this.showOutput = true;
        this.messages.push({ type: 'error', text: `Conversion failed: ${err.message}` });
      } finally {
        this.isConverting = false;
      }
    },

    displayResults(result, options, files) {
      this.showOutput = true;

      if (result.warnings && result.warnings.length > 0) {
        result.warnings.forEach(w => {
          const msgType = w.verbosity === 'INFO' ? 'info' : 'warning';
          this.messages.push({ type: msgType, text: w.pretty || w.message || JSON.stringify(w) });
        });
      }

      if (result.stderr && result.stderr.includes('ERROR')) {
        this.messages.push({ type: 'error', text: result.stderr });
        return;
      }

      const isBinary = this.binaryFormats.includes(options.to);

      if (this.output) {
        // Already set (e.g., pdf via typst)
      } else if (options['output-file'] && files[options['output-file']]) {
        this.output = files[options['output-file']];
        if (!isBinary) {
          const reader = new FileReader();
          reader.onload = () => {
            this.outputPreview = reader.result.substring(0, 50000);
            if (reader.result.length > 50000) {
              this.outputPreview += '\n\n... (truncated)';
            }
          };
          reader.readAsText(this.output);
        }
      } else if (result.stdout) {
        this.output = new Blob([result.stdout], { type: 'text/plain' });
        if (!isBinary) {
          this.outputPreview = result.stdout;
        }
      }

      // Check for extracted media
      if (files['media.zip'] && files['media.zip'].size > 0) {
        this.mediaZip = files['media.zip'];
      }

      // Disable convert button until next change
      this.hasChanges = false;
    },

    download() {
      if (!this.output) return;
      const url = URL.createObjectURL(this.output);
      const a = document.createElement('a');
      a.href = url;
      a.download = this.outputFilenameActual || 'output';
      a.click();
      URL.revokeObjectURL(url);
    },

    downloadMedia() {
      if (!this.mediaZip) return;
      const url = URL.createObjectURL(this.mediaZip);
      const a = document.createElement('a');
      a.href = url;
      a.download = 'media.zip';
      a.click();
      URL.revokeObjectURL(url);
    },

    async copyToClipboard() {
      try {
        await navigator.clipboard.writeText(this.outputPreview);
        this.copyBtnText = '✓ Copied!';
        setTimeout(() => { this.copyBtnText = '📋 Copy to Clipboard'; }, 2000);
      } catch (err) {
        this.messages.push({ type: 'error', text: 'Failed to copy to clipboard' });
      }
    }
  };
};

// Mount Petite Vue after module loads (pandoc.wasm is ready)
// Using mount() without selector scans DOM for v-scope attributes
PetiteVue.createApp().mount();
