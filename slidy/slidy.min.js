/* slidy.js

   Copyright (c) 2005-2010 W3C (MIT, ERCIM, Keio), All Rights Reserved.
   W3C liability, trademark, document use and software licensing
   rules apply, see:

   http://www.w3.org/Consortium/Legal/copyright-documents
   http://www.w3.org/Consortium/Legal/copyright-software

   Defines single name "w3c_slidy" in global namespace
   Adds event handlers without trampling on any others
*/

// the slidy object implementation
var w3c_slidy = {
  // classify which kind of browser we're running under
  ns_pos: (typeof window.pageYOffset!='undefined'),
  khtml: ((navigator.userAgent).indexOf("KHTML") >= 0 ? true : false),
  opera: ((navigator.userAgent).indexOf("Opera") >= 0 ? true : false),
  ipad: ((navigator.userAgent).indexOf("iPad") >= 0 ? true : false),
  iphone: ((navigator.userAgent).indexOf("iPhone") >= 0 ? true : false),
  ie: (typeof document.all != "undefined" && !this.opera),
  ie6: (!this.ns_pos && navigator.userAgent.indexOf("MSIE 6") != -1),
  ie7: (!this.ns_pos && navigator.userAgent.indexOf("MSIE 7") != -1),
  ie8: (!this.ns_pos && navigator.userAgent.indexOf("MSIE 8") != -1),
  ie9: (!this.ns_pos && navigator.userAgent.indexOf("MSIE 9") != -1),
  keyboardless: (this.ipad || this.iphone),

  // are we running as XHTML? (doesn't work on Opera)
  is_xhtml: /xml/.test(document.contentType),

  slide_number: 0, // integer slide count: 0, 1, 2, ...
  slide_number_element: null, // element containing slide number
  slides: [], // set to array of slide div's
  notes: [], // set to array of handout div's
  backgrounds: [], // set to array of background div's
  toolbar: null, // element containing toolbar
  title: null, // document title
  last_shown: null, // last incrementally shown item
  eos: null,  // span element for end of slide indicator
  toc: null, // table of contents
  outline: null, // outline element with the focus
  selected_text_len: 0, // length of drag selection on document
  view_all: 0,  // 1 to view all slides + handouts
  want_toolbar: true,  // user preference to show/hide toolbar
  mouse_click_enabled: true, // enables left click for next slide
  scroll_hack: 0, // IE work around for position: fixed
  disable_slide_click: false,  // used by clicked anchors

  lang: "en", // updated to language specified by html file

  help_anchor: null, // used for keyboard focus hack in showToolbar()
  help_page: "http://www.w3.org/Talks/Tools/Slidy2/help/help.html",
  help_text: "Navigate with mouse click, space bar, Cursor Left/Right, " +
             "or Pg Up and Pg Dn. Use S and B to change font size.",

  size_index: 0,
  size_adjustment: 0,
  sizes:  new Array("10pt", "12pt", "14pt", "16pt", "18pt", "20pt",
                    "22pt", "24pt", "26pt", "28pt", "30pt", "32pt"),

  // needed for efficient resizing
  last_width: 0,
  last_height: 0,


  // Needed for cross browser support for relative width/height on
  // object elements. The work around is to save width/height attributes
  // and then to recompute absolute width/height dimensions on resizing
   objects: [],

  // attach initialiation event handlers
  set_up: function () {
    var init = function() { w3c_slidy.init(); };
    if (typeof window.addEventListener != "undefined")
      window.addEventListener("load", init, false);
    else
      window.attachEvent("onload", init);
  },

  hide_slides: function () {
    if (document.body && !w3c_slidy.initialized)
      document.body.style.visibility = "hidden";
    else
      setTimeout(w3c_slidy.hide_slides, 50);
  },

  // hack to persuade IE to compute correct document height
  // as needed for simulating fixed positioning of toolbar
  ie_hack: function () {
    window.resizeBy(0,-1);
    window.resizeBy(0, 1);
  },

  init: function () {
    //alert("slidy starting test 10");
    document.body.style.visibility = "visible";
    this.init_localization();
    this.add_toolbar();
    this.wrap_implicit_slides();
    this.collect_slides();
    this.collect_notes();
    this.collect_backgrounds();
    this.objects = document.body.getElementsByTagName("object");
    this.patch_anchors();
    this.slide_number = this.find_slide_number(location.href);
    window.offscreenbuffering = true;
    this.size_adjustment = this.find_size_adjust();
    this.time_left = this.find_duration();
    this.hide_image_toolbar();  // suppress IE image toolbar popup
    this.init_outliner();  // activate fold/unfold support
    this.title = document.title;

    // work around for opera bug
    this.is_xhtml = (document.body.tagName == "BODY" ? false : true);

    if (this.slides.length > 0)
    {
      var slide = this.slides[this.slide_number];
   
      if (this.slide_number > 0)
      {
        this.set_visibility_all_incremental("visible");
        this.last_shown = this.previous_incremental_item(null);
        this.set_eos_status(true);
      }
      else
      {
        this.last_shown = null;
        this.set_visibility_all_incremental("hidden");
        this.set_eos_status(!this.next_incremental_item(this.last_shown));
      }

      this.set_location();
      this.add_class(this.slides[0], "first-slide");
      w3c_slidy.show_slide(slide);
    }

    this.toc = this.table_of_contents();

    this.add_initial_prompt();

    // bind event handlers without interfering with custom page scripts
    // Tap events behave too weirdly to support clicks reliably on
    // iPhone and iPad, so exclude these from click handler

    if (!this.keyboardless)
      this.add_listener(document.body, "click", this.mouse_button_click);

    this.add_listener(document, "keydown", this.key_down);
    this.add_listener(document, "keypress", this.key_press);
    this.add_listener(window, "resize", this.resized);
    this.add_listener(window, "scroll", this.scrolled);
    this.add_listener(window, "unload", this.unloaded);

    // this seems to be a debugging hack
    //if (!document.body.onclick)
    //  document.body.onclick = function () { };

    this.single_slide_view();

    //this.set_location();

    this.resized();

    if (this.ie7)
      setTimeout(w3c_slidy.ie_hack, 100);

    this.show_toolbar();

    // for back button detection
    setInterval(function () { w3c_slidy.check_location(); }, 200);
    w3c_slidy.initialized = true;
  },

  // create div element with links to each slide
  table_of_contents: function () {
    var toc = this.create_element("div");
    this.add_class(toc, "slidy_toc hidden");
    //toc.setAttribute("tabindex", "0");

    var heading = this.create_element("div");
    this.add_class(heading, "toc-heading");
    heading.innerHTML = this.localize("Table of Contents");

    toc.appendChild(heading);
    var previous = null;

    for (var i = 0; i < this.slides.length; ++i)
    {
      var title = this.has_class(this.slides[i], "title");
      var num = document.createTextNode((i + 1) + ". ");

      toc.appendChild(num);

      var a = this.create_element("a");
      a.setAttribute("href", "#(" + (i+1) + ")");

      if (title)
        this.add_class(a, "titleslide");

      var name = document.createTextNode(this.slide_name(i));
      a.appendChild(name);
      a.onclick = w3c_slidy.toc_click;
      a.onkeydown = w3c_slidy.toc_key_down;
      a.previous = previous;

      if (previous)
        previous.next = a;

      toc.appendChild(a);

      if (i == 0)
        toc.first = a;

      if (i < this.slides.length - 1)
      {
        var br = this.create_element("br");
        toc.appendChild(br);
      }

      previous = a;
    }

    toc.focus = function () {
      if (this.first)
        this.first.focus();
    }

    toc.onmouseup = w3c_slidy.mouse_button_up;

    toc.onclick = function (e) {
      e||(e=window.event);

      if (w3c_slidy.selected_text_len <= 0)
         w3c_slidy.hide_table_of_contents(true);

      w3c_slidy.stop_propagation(e);
    
      if (e.cancel != undefined)
        e.cancel = true;
      
      if (e.returnValue != undefined)
        e.returnValue = false;
      
      return false;
    };

    document.body.insertBefore(toc, document.body.firstChild);
    return toc;
  },

  is_shown_toc: function () {
    return !w3c_slidy.has_class(w3c_slidy.toc, "hidden");
  },

  show_table_of_contents: function () {
    w3c_slidy.remove_class(w3c_slidy.toc, "hidden");
    var toc = w3c_slidy.toc;
    toc.focus();

    if (w3c_slidy.ie7 && w3c_slidy.slide_number == 0)
      setTimeout(w3c_slidy.ie_hack, 100);
  },

  hide_table_of_contents: function (focus) {
    w3c_slidy.add_class(w3c_slidy.toc, "hidden");

    if (focus && !w3c_slidy.opera)
      w3c_slidy.help_anchor.focus();
  },

  toggle_table_of_contents: function () {
    if (w3c_slidy.is_shown_toc())
      w3c_slidy.hide_table_of_contents(true);
    else
      w3c_slidy.show_table_of_contents();
  },

  // called on clicking toc entry
  toc_click: function (e) {
    if (!e)
      e = window.event;

    var target = w3c_slidy.get_target(e);

    if (target && target.nodeType == 1)
    {
      var uri = target.getAttribute("href");

      if (uri)
      {
        //alert("going to " + uri);
        var slide = w3c_slidy.slides[w3c_slidy.slide_number];
        w3c_slidy.hide_slide(slide);
        w3c_slidy.slide_number = w3c_slidy.find_slide_number(uri);
        slide = w3c_slidy.slides[w3c_slidy.slide_number];
        w3c_slidy.last_shown = null;
        w3c_slidy.set_location();
        w3c_slidy.set_visibility_all_incremental("hidden");
        w3c_slidy.set_eos_status(!w3c_slidy.next_incremental_item(w3c_slidy.last_shown));
        w3c_slidy.show_slide(slide);
        //target.focus();

        try
        {
          if (!w3c_slidy.opera)
            w3c_slidy.help_anchor.focus();
        }
        catch (e)
        {
        }
      }
    }

    w3c_slidy.hide_table_of_contents(true);
    if (w3c_slidy.ie7) w3c_slidy.ie_hack();
    w3c_slidy.stop_propagation(e);
    return w3c_slidy.cancel(e);
  },

  // called onkeydown for toc entry
  toc_key_down: function (event) {
    var key;

    if (!event)
      var event = window.event;

    // kludge around NS/IE differences 
    if (window.event)
      key = window.event.keyCode;
    else if (event.which)
      key = event.which;
    else
      return true; // Yikes! unknown browser

    // ignore event if key value is zero
    // as for alt on Opera and Konqueror
    if (!key)
      return true;

    // check for concurrent control/command/alt key
    // but are these only present on mouse events?

    if (event.ctrlKey || event.altKey)
      return true;

    if (key == 13)
    {
      var uri = this.getAttribute("href");

      if (uri)
      {
        //alert("going to " + uri);
       var slide = w3c_slidy.slides[w3c_slidy.slide_number];
        w3c_slidy.hide_slide(slide);
        w3c_slidy.slide_number = w3c_slidy.find_slide_number(uri);
        slide = w3c_slidy.slides[w3c_slidy.slide_number];
        w3c_slidy.last_shown = null;
        w3c_slidy.set_location();
        w3c_slidy.set_visibility_all_incremental("hidden");
        w3c_slidy.set_eos_status(!w3c_slidy.next_incremental_item(w3c_slidy.last_shown));
        w3c_slidy.show_slide(slide);
        //target.focus();

        try
        {
          if (!w3c_slidy.opera)
            w3c_slidy.help_anchor.focus();
        }
        catch (e)
        {
        }
      }

      w3c_slidy.hide_table_of_contents(true);

      if (self.ie7)
       w3c_slidy.ie_hack();

      return w3c_slidy.cancel(event);
    }

    if (key == 40 && this.next)
    {
      this.next.focus();
      return w3c_slidy.cancel(event);
    }

    if (key == 38 && this.previous)
    {
      this.previous.focus();
      return w3c_slidy.cancel(event);
    }

    return true;
  },


  // ### OBSOLETE ###
  before_print: function () {
    this.show_all_slides();
    this.hide_toolbar();
    alert("before print");
  },

  // ### OBSOLETE ###
  after_print: function () {
    if (!this.view_all)
    {
      this.single_slide_view();
      this.show_toolbar();
    }
    alert("after print");
  },

  // ### OBSOLETE ###
  print_slides: function () {
    this.before_print();
    window.print();
    this.after_print();
  },

  // ### OBSOLETE ?? ###
  toggle_view: function () {
    if (this.view_all)
    {
      this.single_slide_view();
      this.show_toolbar();
      this.view_all = 0;
    }
    else
    {
      this.show_all_slides();
      this.hide_toolbar();
      this.view_all = 1;
    }
  },

  // prepare for printing  ### OBSOLETE ###
  show_all_slides: function () {
    this.remove_class(document.body, "single_slide");
    this.set_visibility_all_incremental("visible");
  },

  // restore after printing  ### OBSOLETE ###
  single_slide_view: function () {
    this.add_class(document.body, "single_slide");
    this.set_visibility_all_incremental("visible");
    this.last_shown = this.previous_incremental_item(null);
  },

  // suppress IE's image toolbar pop up
  hide_image_toolbar: function () {
    if (!this.ns_pos)
    {
      var images = document.getElementsByTagName("IMG");

      for (var i = 0; i < images.length; ++i)
        images[i].setAttribute("galleryimg", "no");
    }
  },

  unloaded: function (e) {
    //alert("unloaded");
  },

  // Safari and Konqueror don't yet support getComputedStyle()
  // and they always reload page when location.href is updated
  is_KHTML: function () {
    var agent = navigator.userAgent;
    return (agent.indexOf("KHTML") >= 0 ? true : false);
  },

  // find slide name from first h1 element
  // default to document title + slide number
  slide_name: function (index) {
    var name = null;
    var slide = this.slides[index];

    var heading = this.find_heading(slide);

    if (heading)
      name = this.extract_text(heading);

    if (!name)
      name = this.title + "(" + (index + 1) + ")";

    name.replace(/\&/g, "&amp;");
    name.replace(/\</g, "&lt;");
    name.replace(/\>/g, "&gt;");

    return name;
  },

  // find first h1 element in DOM tree
  find_heading: function (node) {
    if (!node || node.nodeType != 1)
      return null;

    if (node.nodeName == "H1" || node.nodeName == "h1")
      return node;

    var child = node.firstChild;

    while (child)
    {
      node = this.find_heading(child);

      if (node)
        return node;

      child = child.nextSibling;
    }

    return null;
  },

  // recursively extract text from DOM tree
  extract_text: function (node) {
    if (!node)
      return "";

    // text nodes
    if (node.nodeType == 3)
      return node.nodeValue;

    // elements
    if (node.nodeType == 1)
    {
      node = node.firstChild;
      var text = "";

      while (node)
      {
        text = text + this.extract_text(node);
        node = node.nextSibling;
      }

      return text;
    }

    return "";
  },

  // find copyright text from meta element
  find_copyright: function () {
    var name, content;
    var meta = document.getElementsByTagName("meta");

    for (var i = 0; i < meta.length; ++i)
    {
      name = meta[i].getAttribute("name");
      content = meta[i].getAttribute("content");

      if (name == "copyright")
        return content;
    }

    return null;
  },

  find_size_adjust: function () {
    var name, content, offset;
    var meta = document.getElementsByTagName("meta");

    for (var i = 0; i < meta.length; ++i)
    {
      name = meta[i].getAttribute("name");
      content = meta[i].getAttribute("content");

      if (name == "font-size-adjustment")
        return 1 * content;
    }

    return 1;
  },

  // <meta name="duration" content="20" />  for 20 minutes
  find_duration: function () {
    var name, content, offset;
    var meta = document.getElementsByTagName("meta");

    for (var i = 0; i < meta.length; ++i)
    {
      name = meta[i].getAttribute("name");
      content = meta[i].getAttribute("content");

      if (name == "duration")
        return 60000 * content;
    }

    return null;
  },

  replace_by_non_breaking_space: function (str) {
    for (var i = 0; i < str.length; ++i)
      str[i] = 160;
  },

  // ### CHECK ME ### is use of "li" okay for text/html?
  // for XHTML do we also need to specify namespace?
  init_outliner: function () {
    var items = document.getElementsByTagName("li");

    for (var i = 0; i < items.length; ++i)
    {
      var target = items[i];

      if (!this.has_class(target.parentNode, "outline"))
        continue;

      target.onclick = this.outline_click;
/* ### more work needed for IE6
      if (!this.ns_pos)
      {
        target.onmouseover = this.hover_outline;
        target.onmouseout = this.unhover_outline;
      }
*/
      if (this.foldable(target))
      {
        target.foldable = true;
        target.onfocus = function () {w3c_slidy.outline = this;};
        target.onblur = function () {w3c_slidy.outline = null;};

        if (!target.getAttribute("tabindex"))
          target.setAttribute("tabindex", "0");

        if (this.has_class(target, "expand"))
          this.unfold(target);
        else
          this.fold(target);
      }
      else
      {
        this.add_class(target, "nofold");
        target.visible = true;
        target.foldable = false;
      }
    }
  },

  foldable: function (item) {
    if (!item || item.nodeType != 1)
      return false;

    var node = item.firstChild;

    while (node)
    {
      if (node.nodeType == 1 && this.is_block(node))
        return true;

      node = node.nextSibling;
    }

    return false;
  },

  // ### CHECK ME ### switch to add/remove "hidden" class
  fold: function (item) {
    if (item)
    {
      this.remove_class(item, "unfolded");
      this.add_class(item, "folded");
    }

    var node = item ? item.firstChild : null;

    while (node)
    {
      if (node.nodeType == 1 && this.is_block(node)) // element
      {
         w3c_slidy.add_class(node, "hidden");
      }

      node = node.nextSibling;
    }

    item.visible = false;
  },

  // ### CHECK ME ### switch to add/remove "hidden" class
  unfold: function (item) {
    if (item)
    {
      this.add_class(item, "unfolded");
      this.remove_class(item, "folded");
    }

    var node = item ? item.firstChild : null;

    while (node)
    {
      if (node.nodeType == 1 && this.is_block(node)) // element
      {
        w3c_slidy.remove_class(node, "hidden");
      }

      node = node.nextSibling;
    }

    item.visible = true;
  },

  outline_click: function (e) {
    if (!e)
      e = window.event;

    var rightclick = false;
    var target = w3c_slidy.get_target(e);

    while (target && target.visible == undefined)
      target = target.parentNode;

    if (!target)
      return true;

    if (e.which)
      rightclick = (e.which == 3);
    else if (e.button)
      rightclick = (e.button == 2);

    if (!rightclick && target.visible != undefined)
    {
      if (target.foldable)
      {
        if (target.visible)
          w3c_slidy.fold(target);
        else
          w3c_slidy.unfold(target);
      }

      w3c_slidy.stop_propagation(e);
      e.cancel = true;
      e.returnValue = false;
    }

    return false;
  },

  add_initial_prompt: function () {
    var prompt = this.create_element("div");
    prompt.setAttribute("class", "initial_prompt");

    var p1 = this.create_element("p");
    prompt.appendChild(p1);
    p1.setAttribute("class", "help");

    if (this.keyboardless)
      p1.innerHTML = "Tap footer to move to next slide";
    else
      p1.innerHTML = "Space or Right Arrow to move to next " +
                     "slide, click help below for more details";

    this.add_listener(prompt, "click", function (e) {
      document.body.removeChild(prompt);
      w3c_slidy.stop_propagation(e);
    
      if (e.cancel != undefined)
        e.cancel = true;
      
      if (e.returnValue != undefined)
        e.returnValue = false;
      
      return false;
    });

    document.body.appendChild(prompt);
    this.initial_prompt = prompt;
    setTimeout(function() {document.body.removeChild(prompt);}, 5000);
  },

  add_toolbar: function () {
    var counter, page;

     this.toolbar = this.create_element("div");
     this.toolbar.setAttribute("class", "toolbar");

     // a reasonably behaved browser
     if (this.ns_pos || !this.ie6)
     {
       var right = this.create_element("div");
       right.setAttribute("style", "float: right; text-align: right");

       counter = this.create_element("span")
       counter.innerHTML = this.localize("slide") + " n/m";
       right.appendChild(counter);
       this.toolbar.appendChild(right);

       var left = this.create_element("div");
       left.setAttribute("style", "text-align: left");

       // global end of slide indicator
       this.eos = this.create_element("span");
       this.eos.innerHTML = "* ";
       left.appendChild(this.eos);

       var help = this.create_element("a");
       help.setAttribute("href", this.help_page);
       help.setAttribute("title", this.localize(this.help_text));
       help.innerHTML = this.localize("help?");
       left.appendChild(help);
       this.help_anchor = help;  // save for focus hack

       var gap1 = document.createTextNode(" ");
       left.appendChild(gap1);

       var contents = this.create_element("a");
       contents.setAttribute("href", "javascript:w3c_slidy.toggle_table_of_contents()");
       contents.setAttribute("title", this.localize("table of contents"));
       contents.innerHTML = this.localize("contents?");
       left.appendChild(contents);

       var gap2 = document.createTextNode(" ");
       left.appendChild(gap2);

       var copyright = this.find_copyright();

       if (copyright)
       {
         var span = this.create_element("span");
         span.className = "copyright";
         span.innerHTML = copyright;
         left.appendChild(span);
       }

       this.toolbar.setAttribute("tabindex", "0");
       this.toolbar.appendChild(left);
     }
     else // IE6 so need to work around its poor CSS support
     {
       this.toolbar.style.position = (this.ie7 ? "fixed" : "absolute");
       this.toolbar.style.zIndex = "200";
       this.toolbar.style.width = "99.9%";
       this.toolbar.style.height = "1.2em";
       this.toolbar.style.top = "auto";
       this.toolbar.style.bottom = "0";
       this.toolbar.style.left = "0";
       this.toolbar.style.right = "0";
       this.toolbar.style.textAlign = "left";
       this.toolbar.style.fontSize = "60%";
       this.toolbar.style.color = "red";
       this.toolbar.borderWidth = 0;
       this.toolbar.className = "toolbar";
       this.toolbar.style.background = "rgb(240,240,240)";

       // would like to have help text left aligned
       // and page counter right aligned, floating
       // div's don't work, so instead use nested
       // absolutely positioned div's.

       var sp = this.create_element("span");
       sp.innerHTML = "&nbsp;&nbsp;*&nbsp;";
       this.toolbar.appendChild(sp);
       this.eos = sp;  // end of slide indicator

       var help = this.create_element("a");
       help.setAttribute("href", this.help_page);
       help.setAttribute("title", this.localize(this.help_text));
       help.innerHTML = this.localize("help?");
       this.toolbar.appendChild(help);
       this.help_anchor = help;  // save for focus hack

       var gap1 = document.createTextNode(" ");
       this.toolbar.appendChild(gap1);

       var contents = this.create_element("a");
       contents.setAttribute("href", "javascript:toggleTableOfContents()");
       contents.setAttribute("title", this.localize("table of contents".localize));
       contents.innerHTML = this.localize("contents?");
       this.toolbar.appendChild(contents);

       var gap2 = document.createTextNode(" ");
       this.toolbar.appendChild(gap2);

       var copyright = this.find_copyright();

       if (copyright)
       {
         var span = this.create_element("span");
         span.innerHTML = copyright;
         span.style.color = "black";
         span.style.marginLeft = "0.5em";
         this.toolbar.appendChild(span);
       }

       counter = this.create_element("div")
       counter.style.position = "absolute";
       counter.style.width = "auto"; //"20%";
       counter.style.height = "1.2em";
       counter.style.top = "auto";
       counter.style.bottom = 0;
       counter.style.right = "0";
       counter.style.textAlign = "right";
       counter.style.color = "red";
       counter.style.background = "rgb(240,240,240)";

       counter.innerHTML = this.localize("slide") + " n/m";
       this.toolbar.appendChild(counter);
     }

     // ensure that click isn't passed through to the page
     this.toolbar.onclick =
         function (e) {
           if (!e)
             e = window.event;

           var target = e.target;

           if (!target && e.srcElement)
             target = e.srcElement;

           // work around Safari bug
           if (target && target.nodeType == 3)
             target = target.parentNode;

           w3c_slidy.stop_propagation(e);

           if (target && target.nodeName.toLowerCase() != "a")
             w3c_slidy.mouse_button_click(e);
         };

     this.slide_number_element = counter;
     this.set_eos_status(false);
     document.body.appendChild(this.toolbar);
  },

  // wysiwyg editors make it hard to use div elements
  // e.g. amaya loses the div when you copy and paste
  // this function wraps div elements around implicit
  // slides which start with an h1 element and continue
  // up to the next heading or div element
  wrap_implicit_slides: function () {
    var i, heading, node, next, div;
    var headings = document.getElementsByTagName("h1");

    if (!headings)
      return;

    for (i = 0; i < headings.length; ++i)
    {
      heading = headings[i];

      if (heading.parentNode != document.body)
        continue;

      node = heading.nextSibling;

      div = document.createElement("div");
      this.add_class(div, "slide");
      document.body.replaceChild(div, heading);
      div.appendChild(heading);

      while (node)
      {
        if (node.nodeType == 1 &&    // an element
             (node.nodeName == "H1" ||
              node.nodeName == "h1" ||
              node.nodeName == "DIV" ||
              node.nodeName == "div"))
          break;

        next = node.nextSibling;
        node = document.body.removeChild(node);
        div.appendChild(node);
        node = next;
      } 
    }
  },

// return new array of all slides
  collect_slides: function () {
    var slides = new Array();
    var divs = document.body.getElementsByTagName("div");

    for (var i = 0; i < divs.length; ++i)
    {
      div = divs.item(i);

      if (this.has_class(div, "slide"))
      {
        // add slide to collection
        slides[slides.length] = div;

        // hide each slide as it is found
        this.add_class(div, "hidden");

        // add dummy <br/> at end for scrolling hack
        var node1 = document.createElement("br");
        div.appendChild(node1);
        var node2 = document.createElement("br");
        div.appendChild(node2);
      }
      else if (this.has_class(div, "background"))
      {  // work around for Firefox SVG reload bug
        // which otherwise replaces 1st SVG graphic with 2nd
        div.style.display = "block";
      }
    }

    this.slides = slides;
  },

  // return new array of all <div class="handout">
  collect_notes: function () {
    var notes = new Array();
    var divs = document.body.getElementsByTagName("div");

    for (var i = 0; i < divs.length; ++i)
    {
      div = divs.item(i);

      if (this.has_class(div, "handout"))
      {
        // add note to collection
        notes[notes.length] = div;

        // and hide it
        this.add_class(div, "hidden");
      }
    }

    this.notes = notes;
  },

  // return new array of all <div class="background">
  // including named backgrounds e.g. class="background titlepage"
  collect_backgrounds: function () {
    var backgrounds = new Array();
    var divs = document.body.getElementsByTagName("div");

    for (var i = 0; i < divs.length; ++i)
    {
      div = divs.item(i);

      if (this.has_class(div, "background"))
      {
        // add background to collection
        backgrounds[backgrounds.length] = div;

        // and hide it
        this.add_class(div, "hidden");
      }
    }

    this.backgrounds = backgrounds;
  },

  // set click handlers on all anchors
  patch_anchors: function () {
    var self = w3c_slidy;
    var handler = function (event) {
      // compare this.href with location.href
      // for link to another slide in this doc

      if (self.page_address(this.href) == self.page_address(location.href))
      {
        // yes, so find new slide number
        var newslidenum = self.find_slide_number(this.href);

        if (newslidenum != self.slide_number)
        {
          var slide = self.slides[self.slide_number];
          self.hide_slide(slide);
          self.slide_number = newslidenum;
          slide = self.slides[self.slide_number];
          self.show_slide(slide);
          self.set_location();
        }
      }
      else
        w3c_slidy.stop_propagation(event);

//      else if (this.target == null)
//        location.href = this.href;

      this.blur();
      self.disable_slide_click = true;
    };

    var anchors = document.body.getElementsByTagName("a");

    for (var i = 0; i < anchors.length; ++i)
    {
      if (window.addEventListener)
        anchors[i].addEventListener("click", handler, false);
      else
        anchors[i].attachEvent("onclick", handler);
    }
  },

  // ### CHECK ME ### see which functions are invoked via setTimeout
  // either directly or indirectly for use of w3c_slidy vs this
  show_slide_number: function () {
    var timer = w3c_slidy.get_timer();
    w3c_slidy.slide_number_element.innerHTML = timer + w3c_slidy.localize("slide") + " " +
           (w3c_slidy.slide_number + 1) + "/" + w3c_slidy.slides.length;
  },

  // every 200mS check if the location has been changed as a
  // result of the user activating the Back button/menu item
  // doesn't work for Opera < 9.5
  check_location: function () {
    var hash = location.hash;

    if (w3c_slidy.slide_number > 0 && (hash == "" || hash == "#"))
      w3c_slidy.goto_slide(0);
    else if (hash.length > 2 && hash != "#("+(w3c_slidy.slide_number+1)+")")
    {
      var num = parseInt(location.hash.substr(2));

      if (!isNaN(num))
        w3c_slidy.goto_slide(num-1);
    }

    if (w3c_slidy.time_left && w3c_slidy.slide_number > 0)
    {
      w3c_slidy.show_slide_number();

      if (w3c_slidy.time_left > 0)
        w3c_slidy.time_left -= 200;
    } 
  },

  get_timer: function () {
    var timer = "";
    if (w3c_slidy.time_left)
    {
      var mins, secs;
      secs = Math.floor(w3c_slidy.time_left/1000);
      mins = Math.floor(secs / 60);
      secs = secs % 60;
      timer = (mins ? mins+"m" : "") + secs + "s ";
    }

    return timer;
  },

  // this doesn't push location onto history stack for IE
  // for which a hidden iframe hack is needed: load page into
  // the iframe with script that set's parent's location.hash
  // but that won't work for standalone use unless we can
  // create the page dynamically via a javascript: URL
  set_location: function () {
     var uri = w3c_slidy.page_address(location.href);
     var hash = "#(" + (w3c_slidy.slide_number+1) + ")";

     if (w3c_slidy.slide_number >= 0)
       uri = uri + hash;

     if (w3c_slidy.ie && (w3c_slidy.ie6 || w3c_slidy.ie7))
       w3c_slidy.push_hash(hash);

     if (uri != location.href) // && !khtml
        location.href = uri;

     if (this.khtml)
        hash = "(" + (w3c_slidy.slide_number+1) + ")";

     if (!this.ie && location.hash != hash && location.hash != "")
       location.hash = hash;

     document.title = w3c_slidy.title + " (" + (w3c_slidy.slide_number+1) + ")";
     w3c_slidy.show_slide_number();
  },

  page_address: function (uri) {
    var i = uri.indexOf("#");

    if (i < 0)
      i = uri.indexOf("%23");

    // check if anchor is entire page

    if (i < 0)
      return uri;  // yes

    return uri.substr(0, i);
  },

  // only used for IE6 and IE7
  on_frame_loaded: function (hash) {
    location.hash = hash;
    var uri = w3c_slidy.page_address(location.href);
    location.href = uri + hash;
  },

  // history hack with thanks to Bertrand Le Roy
  push_hash: function (hash) {
    if (hash == "") hash = "#(1)";
      window.location.hash = hash;

    var doc = document.getElementById("historyFrame").contentWindow.document;
    doc.open("javascript:'<html></html>'");
    doc.write("<html><head><script type=\"text/javascript\">window.parent.w3c_slidy.on_frame_loaded('"+
      (hash) + "');</script></head><body>hello mum</body></html>");
      doc.close();
  },

  // find current slide based upon location
  // first find target anchor and then look
  // for associated div element enclosing it
  // finally map that to slide number
  find_slide_number: function (uri) {
    // first get anchor from page location

    var i = uri.indexOf("#");

    // check if anchor is entire page
    if (i < 0)
      return 0;  // yes

    var anchor = unescape(uri.substr(i+1));

    // now use anchor as XML ID to find target
    var target = document.getElementById(anchor);

    if (!target)
    {
      // does anchor look like "(2)" for slide 2 ??
      // where first slide is (1)
      var re = /\((\d)+\)/;

      if (anchor.match(re))
      {
        var num = parseInt(anchor.substring(1, anchor.length-1));

        if (num > this.slides.length)
          num = 1;

        if (--num < 0)
          num = 0;

        return num;
      }

      // accept [2] for backwards compatibility
      re = /\[(\d)+\]/;

      if (anchor.match(re))
      {
         var num = parseInt(anchor.substring(1, anchor.length-1));

         if (num > this.slides.length)
            num = 1;

         if (--num < 0)
            num = 0;

         return num;
      }

      // oh dear unknown anchor
      return 0;
    }

    // search for enclosing slide

    while (true)
    {
      // browser coerces html elements to uppercase!
      if (target.nodeName.toLowerCase() == "div" &&
            this.has_class(target, "slide"))
      {
        // found the slide element
        break;
      }

      // otherwise try parent element if any

      target = target.parentNode;

      if (!target)
      {
        return 0;   // no luck!
      }
    };

    for (i = 0; i < slides.length; ++i)
    {
      if (slides[i] == target)
        return i;  // success
    }

    // oh dear still no luck
    return 0;
  },

  previous_slide: function (incremental) {
    if (!w3c_slidy.view_all)
    {
      var slide;

      if ((incremental || w3c_slidy.slide_number == 0) && w3c_slidy.last_shown != null)
      {
        w3c_slidy.last_shown = w3c_slidy.hide_previous_item(w3c_slidy.last_shown);
        w3c_slidy.set_eos_status(false);
      }
      else if (w3c_slidy.slide_number > 0)
      {
        slide = w3c_slidy.slides[w3c_slidy.slide_number];
        w3c_slidy.hide_slide(slide);

        w3c_slidy.slide_number = w3c_slidy.slide_number - 1;
        slide = w3c_slidy.slides[w3c_slidy.slide_number];
        w3c_slidy.set_visibility_all_incremental("visible");
        w3c_slidy.last_shown = w3c_slidy.previous_incremental_item(null);
        w3c_slidy.set_eos_status(true);
        w3c_slidy.show_slide(slide);
      }

      w3c_slidy.set_location();

      if (!w3c_slidy.ns_pos)
        w3c_slidy.refresh_toolbar(200);
    }
  },

  next_slide: function (incremental) {
    if (!w3c_slidy.view_all)
    {
      var slide, last = w3c_slidy.last_shown;

      if (incremental || w3c_slidy.slide_number == w3c_slidy.slides.length - 1)
         w3c_slidy.last_shown = w3c_slidy.reveal_next_item(w3c_slidy.last_shown);

      if ((!incremental || w3c_slidy.last_shown == null) &&
             w3c_slidy.slide_number < w3c_slidy.slides.length - 1)
      {
         slide = w3c_slidy.slides[w3c_slidy.slide_number];
         w3c_slidy.hide_slide(slide);

         w3c_slidy.slide_number = w3c_slidy.slide_number + 1;
         slide = w3c_slidy.slides[w3c_slidy.slide_number];
         w3c_slidy.last_shown = null;
         w3c_slidy.set_visibility_all_incremental("hidden");
         w3c_slidy.show_slide(slide);
      }
      else if (!w3c_slidy.last_shown)
      {
         if (last && incremental)
           w3c_slidy.last_shown = last;
      }

      w3c_slidy.set_location();

      w3c_slidy.set_eos_status(!w3c_slidy.next_incremental_item(w3c_slidy.last_shown));

      if (!w3c_slidy.ns_pos)
         w3c_slidy.refresh_toolbar(200);
     }
  },

  // to first slide with nothing revealed
  // i.e. state at start of presentation
  first_slide: function () {
     if (!w3c_slidy.view_all)
     {
       var slide;

       if (w3c_slidy.slide_number != 0)
       {
         slide = w3c_slidy.slides[w3c_slidy.slide_number];
         w3c_slidy.hide_slide(slide);

         w3c_slidy.slide_number = 0;
         slide = w3c_slidy.slides[w3c_slidy.slide_number];
         w3c_slidy.last_shown = null;
         w3c_slidy.set_visibility_all_incremental("hidden");
         w3c_slidy.show_slide(slide);
       }

       w3c_slidy.set_eos_status(
         !w3c_slidy.next_incremental_item(w3c_slidy.last_shown));
       w3c_slidy.set_location();
     }
  },

  // goto last slide with everything revealed
  // i.e. state at end of presentation
  last_slide: function () {
    if (!w3c_slidy.view_all)
    {
      var slide;

      w3c_slidy.last_shown = null; //revealNextItem(lastShown);

      if (w3c_slidy.last_shown == null &&
          w3c_slidy.slide_number < w3c_slidy.slides.length - 1)
      {
         slide = w3c_slidy.slides[w3c_slidy.slide_number];
         w3c_slidy.hide_slide(slide);
         w3c_slidy.slide_number = w3c_slidy.slides.length - 1;
         slide = w3c_slidy.slides[w3c_slidy.slide_number];
         w3c_slidy.set_visibility_all_incremental("visible");
         w3c_slidy.last_shown = w3c_slidy.previous_incremental_item(null);

         w3c_slidy.show_slide(slide);
      }
      else
      {
         w3c_slidy.set_visibility_all_incremental("visible");
         w3c_slidy.last_shown = w3c_slidy.previous_incremental_item(null);
      }

      w3c_slidy.set_eos_status(true);
      w3c_slidy.set_location();
    }
  },


  // ### check this and consider add/remove class
  set_eos_status: function (state) {
    if (this.eos)
      this.eos.style.color = (state ? "rgb(240,240,240)" : "red");
  },

  // first slide is 0
  goto_slide: function (num) {
    //alert("going to slide " + (num+1));
    var slide = w3c_slidy.slides[w3c_slidy.slide_number];
    w3c_slidy.hide_slide(slide);
    w3c_slidy.slide_number = num;
    slide = w3c_slidy.slides[w3c_slidy.slide_number];
    w3c_slidy.last_shown = null;
    w3c_slidy.set_visibility_all_incremental("hidden");
    w3c_slidy.set_eos_status(!w3c_slidy.next_incremental_item(w3c_slidy.last_shown));
    document.title = w3c_slidy.title + " (" + (w3c_slidy.slide_number+1) + ")";
    w3c_slidy.show_slide(slide);
    w3c_slidy.show_slide_number();
  },


  show_slide: function (slide) {
    this.sync_background(slide);
    window.scrollTo(0,0);
    this.remove_class(slide, "hidden");
  },

  hide_slide: function (slide) {
    this.add_class(slide, "hidden");
  },

  // show just the backgrounds pertinent to this slide
  // when slide background-color is transparent
  // this should now work with rgba color values
  sync_background: function (slide) {
    var background;
    var bgColor;

    if (slide.currentStyle)
      bgColor = slide.currentStyle["backgroundColor"];
    else if (document.defaultView)
    {
      var styles = document.defaultView.getComputedStyle(slide,null);

      if (styles)
        bgColor = styles.getPropertyValue("background-color");
      else // broken implementation probably due Safari or Konqueror
      {
        //alert("defective implementation of getComputedStyle()");
        bgColor = "transparent";
      }
    }
    else
      bgColor == "transparent";

    if (bgColor == "transparent" ||
        bgColor.indexOf("rgba") >= 0 ||
        bgColor.indexOf("opacity") >= 0)
    {
      var slideClass = this.get_class_list(slide);

      for (var i = 0; i < this.backgrounds.length; i++)
      {
        background = this.backgrounds[i];

        var bgClass = this.get_class_list(background);

        if (this.matching_background(slideClass, bgClass))
          this.remove_class(background, "hidden");
        else
          this.add_class(background, "hidden");
      }
    }
    else // forcibly hide all backgrounds
      this.hide_backgrounds();
  },

  hide_backgrounds: function () {
    for (var i = 0; i < this.backgrounds.length; i++)
    {
      background = this.backgrounds[i];
      this.add_class(background, "hidden");
    }
  },

  // compare classes for slide and background
  matching_background: function (slideClass, bgClass) {
    var i, count, pattern, result;

    // define pattern as regular expression
    pattern = /\w+/g;

    // check background class names
    result = bgClass.match(pattern);

    for (i = count = 0; i < result.length; i++)
    {
      if (result[i] == "hidden")
        continue;

      if (result[i] == "background")
	continue;

      ++count;
    }

    if (count == 0)  // default match
      return true;

    // check for matches and place result in array
    result = slideClass.match(pattern);

    // now check if desired name is present for background
    for (i = count = 0; i < result.length; i++)
    {
      if (result[i] == "hidden")
        continue;

      if (this.has_token(bgClass, result[i]))
        return true;
    }

    return false;
  },

  resized: function () {
     var width = 0;

     if ( typeof( window.innerWidth ) == 'number' )
       width = window.innerWidth;  // Non IE browser
     else if (document.documentElement && document.documentElement.clientWidth)
       width = document.documentElement.clientWidth;  // IE6
     else if (document.body && document.body.clientWidth)
       width = document.body.clientWidth; // IE4

     var height = 0;

     if ( typeof( window.innerHeight ) == 'number' )
       height = window.innerHeight;  // Non IE browser
     else if (document.documentElement && document.documentElement.clientHeight)
       height = document.documentElement.clientHeight;  // IE6
     else if (document.body && document.body.clientHeight)
       height = document.body.clientHeight; // IE4

     if (height && (width/height > 1.05*1024/768))
     {
       width = height * 1024.0/768;
     }

     // IE fires onresize even when only font size is changed!
     // so we do a check to avoid blocking < and > actions
     if (width != w3c_slidy.last_width || height != w3c_slidy.last_height)
     {
       if (width >= 1100)
         w3c_slidy.size_index = 5;    // 4
       else if (width >= 1000)
         w3c_slidy.size_index = 4;    // 3
       else if (width >= 800)
         w3c_slidy.size_index = 3;    // 2
       else if (width >= 600)
         w3c_slidy.size_index = 2;    // 1
       else if (width)
         w3c_slidy.size_index = 0;

       // add in font size adjustment from meta element e.g.
       // <meta name="font-size-adjustment" content="-2" />
       // useful when slides have too much content ;-)

       if (0 <= w3c_slidy.size_index + w3c_slidy.size_adjustment &&
             w3c_slidy.size_index + w3c_slidy.size_adjustment < w3c_slidy.sizes.length)
         w3c_slidy.size_index = w3c_slidy.size_index + w3c_slidy.size_adjustment;

       // enables cross browser use of relative width/height
       // on object elements for use with SVG and Flash media
       w3c_slidy.adjust_object_dimensions(width, height);

       if (document.body.style.fontSize != w3c_slidy.sizes[w3c_slidy.size_index])
       {
         document.body.style.fontSize = w3c_slidy.sizes[w3c_slidy.size_index];
       }

       w3c_slidy.last_width = width;
       w3c_slidy.last_height = height;

       // force reflow to work around Mozilla bug
       if (w3c_slidy.ns_pos)
       {
         var slide = w3c_slidy.slides[w3c_slidy.slide_number];
         w3c_slidy.hide_slide(slide);
         w3c_slidy.show_slide(slide);
       }

       // force correct positioning of toolbar
       w3c_slidy.refresh_toolbar(200);
     }
  },

  scrolled: function () {
    if (w3c_slidy.toolbar && !w3c_slidy.ns_pos && !w3c_slidy.ie7)
    {
      w3c_slidy.hack_offset = w3c_slidy.scroll_x_offset();
      // hide toolbar
      w3c_slidy.toolbar.style.display = "none";

      // make it reappear later
      if (w3c_slidy.scrollhack == 0 && !w3c_slidy.view_all)
      {
        setTimeout(function () {w3c_slidy.show_toolbar(); }, 1000);
        w3c_slidy.scrollhack = 1;
      }
    }
  },

  hide_toolbar: function () {
    w3c_slidy.add_class(w3c_slidy.toolbar, "hidden");
    window.focus();
  },

  // used to ensure IE refreshes toolbar in correct position
  refresh_toolbar: function (interval) {
    if (!w3c_slidy.ns_pos && !w3c_slidy.ie7)
    {
      w3c_slidy.hide_toolbar();
      setTimeout(function () {w3c_slidy.show_toolbar(); }, interval);
    }
  },

  // restores toolbar after short delay
  show_toolbar: function () {
    if (w3c_slidy.want_toolbar)
    {
      w3c_slidy.toolbar.style.display = "block";

      if (!w3c_slidy.ns_pos)
      {
        // adjust position to allow for scrolling
        var xoffset = w3c_slidy.scroll_x_offset();
        w3c_slidy.toolbar.style.left = xoffset;
        w3c_slidy.toolbar.style.right = xoffset;

        // determine vertical scroll offset
        //var yoffset = scrollYOffset();

        // bottom is doc height - window height - scroll offset
        //var bottom = documentHeight() - lastHeight - yoffset

        //if (yoffset > 0 || documentHeight() > lastHeight)
        //   bottom += 16;  // allow for height of scrollbar

        w3c_slidy.toolbar.style.bottom = 0; //bottom;
      }

      w3c_slidy.remove_class(w3c_slidy.toolbar, "hidden");
    }

    w3c_slidy.scrollhack = 0;


    // set the keyboard focus to the help link on the
    // toolbar to ensure that document has the focus
    // IE doesn't always work with window.focus()
    // and this hack has benefit of Enter for help

    try
    {
      if (!w3c_slidy.opera)
        w3c_slidy.help_anchor.focus();
    }
    catch (e)
    {
    }
  },

// invoked via F key
  toggle_toolbar: function () {
    if (!w3c_slidy.view_all)
    {
      if (w3c_slidy.has_class(w3c_slidy.toolbar, "hidden"))
      {
        w3c_slidy.remove_class(w3c_slidy.toolbar, "hidden")
        w3c_slidy.want_toolbar = 1;
      }
      else
      {
        w3c_slidy.add_class(w3c_slidy.toolbar, "hidden")
        w3c_slidy.want_toolbar = 0;
      }
    }
  },

  scroll_x_offset: function () {
    if (window.pageXOffset)
      return self.pageXOffset;

    if (document.documentElement && 
             document.documentElement.scrollLeft)
      return document.documentElement.scrollLeft;

    if (document.body)
      return document.body.scrollLeft;

    return 0;
  },

  scroll_y_offset: function () {
    if (window.pageYOffset)
      return self.pageYOffset;

    if (document.documentElement && 
             document.documentElement.scrollTop)
      return document.documentElement.scrollTop;

    if (document.body)
      return document.body.scrollTop;

    return 0;
  },

  // looking for a way to determine height of slide content
  // the slide itself is set to the height of the window
  optimize_font_size: function () {
    var slide = w3c_slidy.slides[w3c_slidy.slide_number];

    //var dh = documentHeight(); //getDocHeight(document);
    var dh = slide.scrollHeight;
    var wh = getWindowHeight();
    var u = 100 * dh / wh;

    alert("window utilization = " + u + "% (doc "
      + dh + " win " + wh + ")");
  },

  // from document object
  get_doc_height: function (doc) {
    if (!doc)
      doc = document;

    if (doc && doc.body && doc.body.offsetHeight)
      return doc.body.offsetHeight;  // ns/gecko syntax

    if (doc && doc.body && doc.body.scrollHeight)
      return doc.body.scrollHeight;

    alert("couldn't determine document height");
  },

  get_window_height: function () {
    if ( typeof( window.innerHeight ) == 'number' )
      return window.innerHeight;  // Non IE browser

    if (document.documentElement && document.documentElement.clientHeight)
      return document.documentElement.clientHeight;  // IE6

    if (document.body && document.body.clientHeight)
      return document.body.clientHeight; // IE4
  },

  document_height: function () {
    var sh, oh;

    sh = document.body.scrollHeight;
    oh = document.body.offsetHeight;

    if (sh && oh)
    {
      return (sh > oh ? sh : oh);
    }

    // no idea!
    return 0;
  },

  smaller: function () {
    if (w3c_slidy.size_index > 0)
    {
      --w3c_slidy.size_index;
    }

    w3c_slidy.toolbar.style.display = "none";
    document.body.style.fontSize = w3c_slidy.sizes[w3c_slidy.size_index];
    var slide = w3c_slidy.slides[w3c_slidy.slide_number];
    w3c_slidy.hide_slide(slide);
    w3c_slidy.show_slide(slide);
    setTimeout(function () {w3c_slidy.show_toolbar(); }, 50);
  },

  bigger: function () {
    if (w3c_slidy.size_index < w3c_slidy.sizes.length - 1)
    {
      ++w3c_slidy.size_index;
    }

    w3c_slidy.toolbar.style.display = "none";
    document.body.style.fontSize = w3c_slidy.sizes[w3c_slidy.size_index];
    var slide = w3c_slidy.slides[w3c_slidy.slide_number];
    w3c_slidy.hide_slide(slide);
    w3c_slidy.show_slide(slide);
    setTimeout(function () {w3c_slidy.show_toolbar(); }, 50);
  },

  // enables cross browser use of relative width/height
  // on object elements for use with SVG and Flash media
  // with thanks to Ivan Herman for the suggestion
  adjust_object_dimensions: function (width, height) {
    for( var i = 0; i < w3c_slidy.objects.length; i++ )
    {
      var obj = this.objects[i];
      var mimeType = obj.getAttribute("type");

      if (mimeType == "image/svg+xml" || mimeType == "application/x-shockwave-flash")
      {
        if ( !obj.initialWidth ) 
          obj.initialWidth = obj.getAttribute("width");

        if ( !obj.initialHeight ) 
          obj.initialHeight = obj.getAttribute("height");

        if ( obj.initialWidth && obj.initialWidth.charAt(obj.initialWidth.length-1) == "%" )
        {
          var w = parseInt(obj.initialWidth.slice(0, obj.initialWidth.length-1));
          var newW = width * (w/100.0);
          obj.setAttribute("width",newW);
        }

        if ( obj.initialHeight &&
             obj.initialHeight.charAt(obj.initialHeight.length-1) == "%" )
        {
          var h = parseInt(obj.initialHeight.slice(0, obj.initialHeight.length-1));
          var newH = height * (h/100.0);
          obj.setAttribute("height", newH);
        }
      }
    }
  },

  // needed for Opera to inhibit default behavior
  // since Opera delivers keyPress even if keyDown
  // was cancelled
  key_press: function (event) {
    if (!event)
      event = window.event;

    if (!w3c_slidy.key_wanted)
      return w3c_slidy.cancel(event);

    return true;
  },

  //  See e.g. http://www.quirksmode.org/js/events/keys.html for keycodes
  key_down: function (event) {
    var key, target, tag;

    w3c_slidy.key_wanted = true;

    if (!event)
      event = window.event;

    // kludge around NS/IE differences 
    if (window.event)
    {
      key = window.event.keyCode;
      target = window.event.srcElement;
    }
    else if (event.which)
    {
      key = event.which;
      target = event.target;
    }
    else
      return true; // Yikes! unknown browser

    // ignore event if key value is zero
    // as for alt on Opera and Konqueror
    if (!key)
       return true;

    // avoid interfering with keystroke
    // behavior for non-slidy chrome elements
    if (!w3c_slidy.slidy_chrome(target) &&
        w3c_slidy.special_element(target))
      return true;

    // check for concurrent control/command/alt key
    // but are these only present on mouse events?

    if (event.ctrlKey || event.altKey || event.metaKey)
       return true;

    // dismiss table of contents if visible
    if (w3c_slidy.is_shown_toc() && key != 9 && key != 16 && key != 38 && key != 40)
    {
      w3c_slidy.hide_table_of_contents(true);

      if (key == 27 || key == 84 || key == 67)
        return w3c_slidy.cancel(event);
    }

    if (key == 34) // Page Down
    {
      if (w3c_slidy.view_all)
        return true;

      w3c_slidy.next_slide(false);
      return w3c_slidy.cancel(event);
    }
    else if (key == 33) // Page Up
    {
      if (w3c_slidy.view_all)
        return true;

      w3c_slidy.previous_slide(false);
      return w3c_slidy.cancel(event);
    }
    else if (key == 32) // space bar
    {
      w3c_slidy.next_slide(true);
      return w3c_slidy.cancel(event);
    }
    else if (key == 37) // Left arrow
    {
      w3c_slidy.previous_slide(!event.shiftKey);
      return w3c_slidy.cancel(event);
    }
    else if (key == 36) // Home
    {
      w3c_slidy.first_slide();
      return w3c_slidy.cancel(event);
    }
    else if (key == 35) // End
    {
      w3c_slidy.last_slide();
      return w3c_slidy.cancel(event);
    }
    else if (key == 39) // Right arrow
    {
      w3c_slidy.next_slide(!event.shiftKey);
      return w3c_slidy.cancel(event);
    }
    else if (key == 13) // Enter
    {
      if (w3c_slidy.outline)
      {
        if (w3c_slidy.outline.visible)
          w3c_slidy.fold(w3c_slidy.outline);
        else
          w3c_slidy.unfold(w3c_slidy.outline);
          
       return w3c_slidy.cancel(event);
      }
    }
    else if (key == 188)  // < for smaller fonts
    {
      w3c_slidy.smaller();
      return w3c_slidy.cancel(event);
    }
    else if (key == 190)  // > for larger fonts
    {
      w3c_slidy.bigger();
      return w3c_slidy.cancel(event);
    }
    else if (key == 189 || key == 109)  // - for smaller fonts
    {
      w3c_slidy.smaller();
      return w3c_slidy.cancel(event);
    }
    else if (key == 187 || key == 191 || key == 107)  // = +  for larger fonts
    {
      w3c_slidy.bigger();
      return w3c_slidy.cancel(event);
    }
    else if (key == 83)  // S for smaller fonts
    {
      w3c_slidy.smaller();
      return w3c_slidy.cancel(event);
    }
    else if (key == 66)  // B for larger fonts
    {
      w3c_slidy.bigger();
      return w3c_slidy.cancel(event);
    }
    else if (key == 90)  // Z for last slide
    {
      w3c_slidy.last_slide();
      return w3c_slidy.cancel(event);
    }
    else if (key == 70)  // F for toggle toolbar
    {
      w3c_slidy.toggle_toolbar();
      return w3c_slidy.cancel(event);
    }
    else if (key == 65)  // A for toggle view single/all slides
    {
      w3c_slidy.toggle_view();
      return w3c_slidy.cancel(event);
    }
    else if (key == 75)  // toggle action of left click for next page
    {
      w3c_slidy.mouse_click_enabled = !w3c_slidy.mouse_click_enabled;
      var alert_msg = (w3c_slidy.mouse_click_enabled ?
                "enabled" : "disabled") +  " mouse click advance";

      alert(w3c_slidy.localize(alert_msg));
      return w3c_slidy.cancel(event);
    }
    else if (key == 84 || key == 67)  // T or C for table of contents
    {
      if (w3c_slidy.toc)
        w3c_slidy.toggle_table_of_contents();

      return w3c_slidy.cancel(event);
    }
    else if (key == 72) // H for help
    {
      window.location = w3c_slidy.help_page;
      return w3c_slidy.cancel(event);
    }
    //else alert("key code is "+ key);

    return true;
  },

  // safe for both text/html and application/xhtml+xml
  create_element: function (name) {
    if (this.xhtml && (typeof document.createElementNS != 'undefined'))
      return document.createElementNS("http://www.w3.org/1999/xhtml", name)

    return document.createElement(name);
  },

  get_element_style: function (elem, IEStyleProp, CSSStyleProp) {
    if (elem.currentStyle)
    {
      return elem.currentStyle[IEStyleProp];
    }
    else if (window.getComputedStyle)
    {
      var compStyle = window.getComputedStyle(elem, "");
      return compStyle.getPropertyValue(CSSStyleProp);
    }
    return "";
  },

  // the string str is a whitespace separated list of tokens
  // test if str contains a particular token, e.g. "slide"
  has_token: function (str, token) {
    if (str)
    {
      // define pattern as regular expression
      var pattern = /\w+/g;

      // check for matches
      // place result in array
      var result = str.match(pattern);

      // now check if desired token is present
      for (var i = 0; i < result.length; i++)
      {
        if (result[i] == token)
          return true;
      }
    }

    return false;
  },

  get_class_list: function (element) {
    if (typeof element.className != 'undefined')
      return element.className;

    return element.getAttribute("class");
  },

  has_class: function (element, name) {
    if (element.nodeType != 1)
      return false;

    var regexp = new RegExp("(^| )" + name + "\W*");

    if (typeof element.className != 'undefined')
      return regexp.test(element.className);

    return regexp.test(element.getAttribute("class"));
  },

  remove_class: function (element, name) {
    var regexp = new RegExp("(^| )" + name + "\W*");
    var clsval = "";

    if (typeof element.className != 'undefined')
    {
      clsval = element.className;

      if (clsval)
      {
        clsval = clsval.replace(regexp, "");
        element.className = clsval;
      }
    }
    else
    {
      clsval = element.getAttribute("class");

      if (clsval)
      {
        clsval = clsval.replace(regexp, "");
        element.setAttribute("class", clsval);
      }
    }
  },

  add_class: function (element, name) {
    if (!this.has_class(element, name))
    {
      if (typeof element.className != 'undefined')
        element.className += " " + name;
      else
      {
        var clsval = element.getAttribute("class");
        clsval = clsval ? clsval + " " + name : name;
        element.setAttribute("class", clsval);
      }
    }
  },

  // HTML elements that can be used with class="incremental"
  // note that you can also put the class on containers like
  // up, ol, dl, and div to make their contents appear
  // incrementally. Upper case is used since this is what
  // browsers report for HTML node names (text/html).
  incremental_elements: null,
  okay_for_incremental: function (name) {
    if (!this.incremental_elements)
    {
      var inclist = new Array();
      inclist["p"] = true;
      inclist["pre"] = true;
      inclist["li"] = true;
      inclist["blockquote"] = true;
      inclist["dt"] = true;
      inclist["dd"] = true;
      inclist["h2"] = true;
      inclist["h3"] = true;
      inclist["h4"] = true;
      inclist["h5"] = true;
      inclist["h6"] = true;
      inclist["span"] = true;
      inclist["address"] = true;
      inclist["table"] = true;
      inclist["tr"] = true;
      inclist["th"] = true;
      inclist["td"] = true;
      inclist["img"] = true;
      inclist["object"] = true;
      this.incremental_elements = inclist;
    }
    return this.incremental_elements[name.toLowerCase()];
  },

  next_incremental_item: function (node) {
    var br = this.is_xhtml ? "br" : "BR";
    var slide = w3c_slidy.slides[w3c_slidy.slide_number];

    for (;;)
    {
      node = w3c_slidy.next_node(slide, node);

      if (node == null || node.parentNode == null)
        break;

      if (node.nodeType == 1)  // ELEMENT
      {
        if (node.nodeName == br)
          continue;

        if (w3c_slidy.has_class(node, "incremental")
             && w3c_slidy.okay_for_incremental(node.nodeName))
          return node;

        if (w3c_slidy.has_class(node.parentNode, "incremental")
             && !w3c_slidy.has_class(node, "non-incremental"))
          return node;
      }
    }

    return node;
  },

  previous_incremental_item: function (node) {
    var br = this.is_xhtml ? "br" : "BR";
    var slide = w3c_slidy.slides[w3c_slidy.slide_number];

    for (;;)
    {
      node = w3c_slidy.previous_node(slide, node);

      if (node == null || node.parentNode == null)
        break;

      if (node.nodeType == 1)
      {
        if (node.nodeName == br)
          continue;

        if (w3c_slidy.has_class(node, "incremental")
             && w3c_slidy.okay_for_incremental(node.nodeName))
          return node;

        if (w3c_slidy.has_class(node.parentNode, "incremental")
             && !w3c_slidy.has_class(node, "non-incremental"))
          return node;
      }
    }

    return node;
  },

  // set visibility for all elements on current slide with
  // a parent element with attribute class="incremental"
  set_visibility_all_incremental: function (value) {
    var node = this.next_incremental_item(null);

    if (value == "hidden")
    {
      while (node)
      {
        w3c_slidy.add_class(node, "invisible");
        node = w3c_slidy.next_incremental_item(node);
      }
    }
    else // value == "visible"
    {
      while (node)
      {
        w3c_slidy.remove_class(node, "invisible");
        node = w3c_slidy.next_incremental_item(node);
      }
    }
  },

  // reveal the next hidden item on the slide
  // node is null or the node that was last revealed
  reveal_next_item: function (node) {
    node = w3c_slidy.next_incremental_item(node);

    if (node && node.nodeType == 1)  // an element
      w3c_slidy.remove_class(node, "invisible");

    return node;
  },

  // exact inverse of revealNextItem(node)
  hide_previous_item: function (node) {
    if (node && node.nodeType == 1)  // an element
      w3c_slidy.add_class(node, "invisible");

    return this.previous_incremental_item(node);
  },

  // left to right traversal of root's content
  next_node: function (root, node) {
    if (node == null)
      return root.firstChild;

    if (node.firstChild)
      return node.firstChild;

    if (node.nextSibling)
      return node.nextSibling;

    for (;;)
    {
      node = node.parentNode;

      if (!node || node == root)
        break;

      if (node && node.nextSibling)
        return node.nextSibling;
    }

    return null;
  },

  // right to left traversal of root's content
  previous_node: function (root, node) {
    if (node == null)
    {
      node = root.lastChild;

      if (node)
      {
        while (node.lastChild)
          node = node.lastChild;
      }

      return node;
    }

    if (node.previousSibling)
    {
      node = node.previousSibling;

      while (node.lastChild)
        node = node.lastChild;

      return node;
    }

    if (node.parentNode != root)
      return node.parentNode;

    return null;
  },

  previous_sibling_element: function (el) {
    el = el.previousSibling;

    while (el && el.nodeType != 1)
      el = el.previousSibling;

    return el;
  },

  next_sibling_element: function (el) {
    el = el.nextSibling;

    while (el && el.nodeType != 1)
      el = el.nextSibling;

    return el;
  },

  first_child_element: function (el) {
    var node;

    for (node = el.firstChild; node; node = node.nextSibling)
    {
      if (node.nodeType == 1)
        break;
    }

    return node;
  },

  first_tag: function (element, tag) {
    var node;

    if (!this.is_xhtml)
      tag = tag.toUpperCase();

    for (node = element.firstChild; node; node = node.nextSibling)
    {
      if (node.nodeType == 1 && node.nodeName == tag)
        break;
    }

    return node;
  },

  hide_selection: function () {
    if (window.getSelection) // Firefox, Chromium, Safari, Opera
    {
      var selection = window.getSelection();

      if (selection.rangeCount > 0)
      {
        var range = selection.getRangeAt(0);
        range.collapse (false);
      }
    }
    else // Internet Explorer
    {
      var textRange = document.selection.createRange ();
      textRange.collapse (false);
    }
  },

  get_selected_text: function () {
    try
    {
      if (window.getSelection)
        return window.getSelection().toString();

      if (document.getSelection)
        return document.getSelection().toString();

      if (document.selection)
        return document.selection.createRange().text;
    }
    catch (e)
    {
    }

    return "";
  },

  // make note of length of selected text
  // as this evaluates to zero in click event
  mouse_button_up: function (e) {
    w3c_slidy.selected_text_len = w3c_slidy.get_selected_text().length;
  },

  // right mouse button click is reserved for context menus
  // it is more reliable to detect rightclick than leftclick
  mouse_button_click: function (e) {
    var rightclick = false;
    var leftclick = false;
    var middleclick = false;
    var target;

    if (!e)
      var e = window.event;

    if (e.target)
      target = e.target;
    else if (e.srcElement)
      target = e.srcElement;

    // work around Safari bug
    if (target.nodeType == 3)
      target = target.parentNode;

    if (e.which) // all browsers except IE
    {
      leftclick = (e.which == 1);
      middleclick = (e.which == 2);
      rightclick = (e.which == 3);
    }
    else if (e.button)
    {
      // Konqueror gives 1 for left, 4 for middle
      // IE6 gives 0 for left and not 1 as I expected

      if (e.button == 4)
        middleclick = true;

      // all browsers agree on 2 for right button
      rightclick = (e.button == 2);
    }
    else
      leftclick = true;

    if (w3c_slidy.selected_text_len > 0)
    {
      w3c_slidy.stop_propagation(e);
      e.cancel = true;
      e.returnValue = false;
      return false;
    }

    // dismiss table of contents
    w3c_slidy.hide_table_of_contents(false);

    // check if target is something that probably want's clicks
    // e.g. a, embed, object, input, textarea, select, option
    var tag = target.nodeName.toLowerCase();

    if (w3c_slidy.mouse_click_enabled && leftclick &&
        !w3c_slidy.special_element(target) &&
        !target.onclick)
    {
      w3c_slidy.next_slide(true);
      w3c_slidy.stop_propagation(e);
      e.cancel = true;
      e.returnValue = false;
      return false;
    }

    return true;
  },

  special_element: function (e) {
    var tag = e.nodeName.toLowerCase();

    return e.onkeydown ||
      e.onclick ||
      tag == "a" ||
      tag == "embed" ||
      tag == "object" ||
      tag == "video" ||
      tag == "audio" ||
      tag == "input" ||
      tag == "textarea" ||
      tag == "select" ||
      tag == "option";
  },

  slidy_chrome: function (el) {
    while (el)
    {
      if (el == w3c_slidy.toc ||
          el == w3c_slidy.toolbar ||
          w3c_slidy.has_class(el, "outline"))
        return true;

      el = el.parentNode;
    }

    return false;
  },

  get_key: function (e)
  {
    var key;

    // kludge around NS/IE differences 
    if (typeof window.event != "undefined")
      key = window.event.keyCode;
    else if (e.which)
      key = e.which;

    return key;
  },

  get_target: function (e) {
    var target;

    if (!e)
      e = window.event;

    if (e.target)
      target = e.target;
    else if (e.srcElement)
      target = e.srcElement;

    if (target.nodeType != 1)
      target = target.parentNode;

    return target;
  },

  // does display property provide correct defaults?
  is_block: function (elem) {
    var tag = elem.nodeName.toLowerCase();

    return tag == "ol" || tag == "ul" || tag == "p" ||
           tag == "li" || tag == "table" || tag == "pre" ||
           tag == "h1" || tag == "h2" || tag == "h3" ||
           tag == "h4" || tag == "h5" || tag == "h6" ||
           tag == "blockquote" || tag == "address"; 
  },

  add_listener: function (element, event, handler) {
    if (window.addEventListener)
      element.addEventListener(event, handler, false);
    else
      element.attachEvent("on"+event, handler);
  },

  // used to prevent event propagation from field controls
  stop_propagation: function (event) {
    event = event ? event : window.event;
    event.cancelBubble = true;  // for IE

    if (event.stopPropagation)
      event.stopPropagation();

    return true;
  },

  cancel: function (event) {
    if (event)
    {
       event.cancel = true;
       event.returnValue = false;

      if (event.preventDefault)
        event.preventDefault();
    }

    w3c_slidy.key_wanted = false;
    return false;
  },

// for each language define an associative array
// and also the help text which is longer

  strings_es: {
    "slide":"pág.",
    "help?":"Ayuda",
    "contents?":"Índice",
    "table of contents":"tabla de contenidos",
    "Table of Contents":"Tabla de Contenidos",
    "restart presentation":"Reiniciar presentación",
    "restart?":"Inicio"
  },
  help_es:
    "Utilice el ratón, barra espaciadora, teclas Izda/Dcha, " +
    "o Re pág y Av pág. Use S y B para cambiar el tamaño de fuente.",

  strings_ca: {
    "slide":"pàg..",
    "help?":"Ajuda",
    "contents?":"Índex",
    "table of contents":"taula de continguts",
    "Table of Contents":"Taula de Continguts",
    "restart presentation":"Reiniciar presentació",
    "restart?":"Inici"
  },
  help_ca:
    "Utilitzi el ratolí, barra espaiadora, tecles Esq./Dta. " +
    "o Re pàg y Av pàg. Usi S i B per canviar grandària de font.",

  strings_cs: {
    "slide":"snímek",
    "help?":"nápověda",
    "contents?":"obsah",
    "table of contents":"obsah prezentace",
    "Table of Contents":"Obsah prezentace",
    "restart presentation":"znovu spustit prezentaci",
    "restart?":"restart"
  },
  help_cs:
    "Prezentaci můžete procházet pomocí kliknutí myši, mezerníku, " +
    "šipek vlevo a vpravo nebo kláves PageUp a PageDown. Písmo se " +
    "dá zvětšit a zmenšit pomocí kláves B a S.",

  strings_nl: {
    "slide":"pagina",
    "help?":"Help?",
    "contents?":"Inhoud?",
    "table of contents":"inhoudsopgave",
    "Table of Contents":"Inhoudsopgave",
    "restart presentation":"herstart presentatie",
    "restart?":"Herstart?"
  },
  help_nl:
     "Navigeer d.m.v. het muis, spatiebar, Links/Rechts toetsen, " +
     "of PgUp en PgDn. Gebruik S en B om de karaktergrootte te veranderen.",

  strings_de: {
    "slide":"Seite",
    "help?":"Hilfe",
    "contents?":"Übersicht",
    "table of contents":"Inhaltsverzeichnis",
    "Table of Contents":"Inhaltsverzeichnis",
    "restart presentation":"Präsentation neu starten",
    "restart?":"Neustart"
  },
  help_de:
    "Benutzen Sie die Maus, Leerschlag, die Cursortasten links/rechts oder " +
    "Page up/Page Down zum Wechseln der Seiten und S und B für die Schriftgrösse.",

  strings_pl: {
    "slide":"slajd",
    "help?":"pomoc?",
    "contents?":"spis treści?",
    "table of contents":"spis treści",
    "Table of Contents":"Spis Treści",
    "restart presentation":"Restartuj prezentację",
    "restart?":"restart?"
  },
  help_pl:
    "Zmieniaj slajdy klikając myszą, naciskając spację, strzałki lewo/prawo" +
    "lub PgUp / PgDn. Użyj klawiszy S i B, aby zmienić rozmiar czczionki.",

  strings_fr: {
    "slide":"page",
    "help?":"Aide",
    "contents?":"Index",
    "table of contents":"table des matières",
    "Table of Contents":"Table des matières",
    "restart presentation":"Recommencer l'exposé",
    "restart?":"Début"
  },
  help_fr:
    "Naviguez avec la souris, la barre d'espace, les flèches " +
    "gauche/droite ou les touches Pg Up, Pg Dn. Utilisez " +
    "les touches S et B pour modifier la taille de la police.",

  strings_hu: {
    "slide":"oldal",
    "help?":"segítség",
    "contents?":"tartalom",
    "table of contents":"tartalomjegyzék",
    "Table of Contents":"Tartalomjegyzék",
    "restart presentation":"bemutató újraindítása",
    "restart?":"újraindítás"
  },
  help_hu:
    "Az oldalak közti lépkedéshez kattintson az egérrel, vagy " +
    "használja a szóköz, a bal, vagy a jobb nyíl, illetve a Page Down, " +
    "Page Up billentyűket. Az S és a B billentyűkkel változtathatja " +
    "a szöveg méretét.",

  strings_it: {
    "slide":"pag.",
    "help?":"Aiuto",
    "contents?":"Indice",
    "table of contents":"indice",
    "Table of Contents":"Indice",
    "restart presentation":"Ricominciare la presentazione",
    "restart?":"Inizio"
  },
  help_it:
    "Navigare con mouse, barra spazio, frecce sinistra/destra o " +
    "PgUp e PgDn. Usare S e B per cambiare la dimensione dei caratteri.",

  strings_el: {
    "slide":"σελίδα",
    "help?":"βοήθεια;",
    "contents?":"περιεχόμενα;",
    "table of contents":"πίνακας περιεχομένων",
    "Table of Contents":"Πίνακας Περιεχομένων",
    "restart presentation":"επανεκκίνηση παρουσίασης",
    "restart?":"επανεκκίνηση;"
  },
  help_el:
    "Πλοηγηθείτε με το κλίκ του ποντικιού, το space, τα βέλη αριστερά/δεξιά, " +
    "ή Page Up και Page Down. Χρησιμοποιήστε τα πλήκτρα S και B για να αλλάξετε " +
    "το μέγεθος της γραμματοσειράς.",

  strings_ja: {
    "slide":"スライド",
    "help?":"ヘルプ",
    "contents?":"目次",
    "table of contents":"目次を表示",
    "Table of Contents":"目次",
    "restart presentation":"最初から再生",
    "restart?":"最初から"
  },
  help_ja:
     "マウス左クリック ・ スペース ・ 左右キー " +
     "または Page Up ・ Page Downで操作， S ・ Bでフォントサイズ変更",

  strings_zh: {
    "slide":"幻灯片",
    "help?":"帮助?",
    "contents?":"内容?",
    "table of contents":"目录",
    "Table of Contents":"目录",
    "restart presentation":"重新启动展示",
    "restart?":"重新启动?"
  },
  help_zh:
    "用鼠标点击, 空格条, 左右箭头, Pg Up 和 Pg Dn 导航. " +
    "用 S, B 改变字体大小.",

  strings_ru: {
    "slide":"слайд",
    "help?":"помощь?",
    "contents?":"содержание?",
    "table of contents":"оглавление",
    "Table of Contents":"Оглавление",
    "restart presentation":"перезапустить презентацию",
    "restart?":"перезапуск?"
  },
  help_ru:
    "Перемещайтесь кликая мышкой, используя клавишу пробел, стрелки" +
    "влево/вправо или Pg Up и Pg Dn. Клавиши S и B меняют размер шрифта.",

  strings_sv: {
    "slide":"sida",
    "help?":"hjälp",
    "contents?":"innehåll",
    "table of contents":"innehållsförteckning",
    "Table of Contents":"Innehållsförteckning",
    "restart presentation":"visa presentationen från början",
    "restart?":"börja om"
  },
  help_sv:
    "Bläddra med ett klick med vänstra musknappen, mellanslagstangenten, " +
    "vänster- och högerpiltangenterna eller tangenterna Pg Up, Pg Dn. " +
    "Använd tangenterna S och B för att ändra textens storlek.",

  strings: { },

  localize: function (src) {
    if (src == "")
      return src;

     // try full language code, e.g. en-US
     var s, lookup = w3c_slidy.strings[w3c_slidy.lang];

     if (lookup)
     {
       s = lookup[src];

       if (s)
        return s;
     }

     // strip country code suffix, e.g.
     // try en if undefined for en-US
     var lg = w3c_slidy.lang.split("-");

     if (lg.length > 1)
     {
       lookup = w3c_slidy.strings[lg[0]];

       if (lookup)
       {
         s = lookup[src];

         if (s)
          return s;
       }
     }

     // otherwise string as is
     return src;
  },

  init_localization: function () {
    var i18n = w3c_slidy;
    var help_text = w3c_slidy.help_text;

    // each such language array is declared in the localize array
    // this is used as in  w3c_slidy.localize("foo");
    this.strings = {
      "es":this.strings_es,
      "ca":this.strings_ca,
      "cs":this.strings_cs,
      "nl":this.strings_nl,
      "de":this.strings_de,
      "pl":this.strings_pl,
      "fr":this.strings_fr,
      "hu":this.strings_hu,
      "it":this.strings_it,
      "el":this.strings_el,
      "jp":this.strings_ja,
      "zh":this.strings_zh,
      "ru":this.strings_ru,
      "sv":this.strings_sv
    },

    i18n.strings_es[help_text] = i18n.help_es;
    i18n.strings_ca[help_text] = i18n.help_ca;
    i18n.strings_cs[help_text] = i18n.help_cs;
    i18n.strings_nl[help_text] = i18n.help_nl;
    i18n.strings_de[help_text] = i18n.help_de;
    i18n.strings_pl[help_text] = i18n.help_pl;
    i18n.strings_fr[help_text] = i18n.help_fr;
    i18n.strings_hu[help_text] = i18n.help_hu;
    i18n.strings_it[help_text] = i18n.help_it;
    i18n.strings_el[help_text] = i18n.help_el;
    i18n.strings_ja[help_text] = i18n.help_ja;
    i18n.strings_zh[help_text] = i18n.help_zh;
    i18n.strings_ru[help_text] = i18n.help_ru;
    i18n.strings_sv[help_text] = i18n.help_sv;

    w3c_slidy.lang = document.body.parentNode.getAttribute("lang");

    if (!w3c_slidy.lang)
      w3c_slidy.lang = document.body.parentNode.getAttribute("xml:lang");

    if (!w3c_slidy.lang)
      w3c_slidy.lang = "en";
  }
};

// hack for back button behavior
if (w3c_slidy.ie6 || w3c_slidy.ie7)
{
  document.write("<iframe id='historyFrame' " +
  "src='javascript:\"<html"+"></"+"html>\"' " +
  "height='1' width='1' " +
  "style='position:absolute;left:-800px'></iframe>");
}

// attach event listeners for initialization
w3c_slidy.set_up();

// hide the slides as soon as body element is available
// to reduce annoying screen mess before the onload event
setTimeout(w3c_slidy.hide_slides, 50);

