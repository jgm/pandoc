/*	This work is licensed under Creative Commons GNU LGPL License.

	License: http://creativecommons.org/licenses/LGPL/2.1/

	Author:  Stefan Goessner/2005-2006
	Web:     http://goessner.net/ 
*/
var Slideous = {
   version: 1.0,
   // == user customisable ===
   clickables: { a: true, button: true, img: true, input: true, object: true, textarea: true, select: true, option: true },
   incrementables: { blockquote: { filter: "self, parent" }, 
                     dd: { filter: "self, parent" },
                     dt: { filter: "self, parent" },
                     h2: { filter: "self, parent" },
                     h3: { filter: "self, parent" },
                     h4: { filter: "self, parent" },
                     h5: { filter: "self, parent" },
                     h6: { filter: "self, parent" },
                     li: { filter: "self, parent" },
                     p: { filter: "self" }, 
                     pre: { filter: "self" }, 
                     img: { filter: "self, parent" }, 
                     object: { filter: "self, parent" },
                     table: { filter: "self, parent" },
                     td: { filter: "self, parent" },
                     th: { filter: "self, parent" },
                     tr: { filter: "parent, grandparent" }
                   },
   autoincrementables: { ol: true, ul: true, dl: true },
   autoincrement: false,
   statusbar: true,
   navbuttons: { incfontbutton:   function(){Slideous.changefontsize(+Slideous.fontdelta);},
                 decfontbutton:   function(){Slideous.changefontsize(-Slideous.fontdelta);},
                 contentbutton:   function(){Slideous.gotoslide(Slideous.tocidx(), true, true);},
                 homebutton:      function(){Slideous.gotoslide(1, true, true);},
                 prevslidebutton: function(){Slideous.previous(false);},
                 previtembutton:  function(){Slideous.previous(true);},
                 nextitembutton:  function(){Slideous.next(true);},
                 nextslidebutton: function(){Slideous.next(false);},
                 endbutton:       function(){Slideous.gotoslide(Slideous.count,true,true);} },
   fontsize: 125,  // in percent, corresponding to body.font-size in css file
   fontdelta: 5,   // increase/decrease fontsize by this value
   mousesensitive: true,
   tocidx: 0,
   tocitems: { toc: "<li><a href=\"#s{\$slideidx}\">{\$slidetitle}</a></li>",
               tocbox: "<option value=\"#s{\$slideidx}\" title=\"{\$slidetitle}\">{\$slidetitle}</option>" },
   keydown: function(evt) {
      evt = evt || window.event;
      var key = evt.keyCode || evt.which;
      if (key && !evt.ctrlKey && !evt.altKey) {
         switch (key) {
            case 33: // page up  ... previous slide
               Slideous.previous(false); evt.cancel = !Slideous.showall; break;
            case 37: // left arrow ... previous item
               Slideous.previous(true); evt.cancel = !Slideous.showall; break;
            case 32: // space bar
            case 39: // right arrow
               Slideous.next(true); evt.cancel = !Slideous.showall; break;
            case 13: // carriage return  ... next slide
            case 34: // page down
               Slideous.next(false); evt.cancel = !Slideous.showall; break;
            case 35: // end  ... last slide (not recognised by opera)
               Slideous.gotoslide(Slideous.count, true, true); evt.cancel = !Slideous.showall; break;
            case 36: // home ... first slide (not recognised by opera)
               Slideous.gotoslide(1, true, true); evt.cancel = !Slideous.showall; break;
            case 65: // A ... show All
            case 80: // P ... Print mode
               Slideous.toggleshowall(!Slideous.showall); evt.cancel = true; break;
            case 67: // C ... goto contents
               Slideous.gotoslide(Slideous.tocidx, true, true); evt.cancel = true; break;
            case 77: // M ... toggle mouse sensitivity
               Slideous.mousenavigation(Slideous.mousesensitive = !Slideous.mousesensitive); evt.cancel = true; break;
            case 83: // S ... toggle statusbar
               Slideous.togglestatusbar(); evt.cancel = true; break;
            case 61:  // + ... increase fontsize
            case 107:
               Slideous.changefontsize(+Slideous.fontdelta); evt.cancel = true; break;
            case 109:  // - ... decrease fontsize
               Slideous.changefontsize(-Slideous.fontdelta); evt.cancel = true; break;
            default: break;
         }
         if (evt.cancel) evt.returnValue = false;
      }
      return !evt.cancel;
   },

   // == program logic ===
   count: 0,                       // # of slides ..
   curidx: 0,                      // current slide index ..
   mousedownpos: null,             // last mouse down position ..
   contentselected: false,         // indicates content selection ..
   showall: true,
   init: function() {
      Slideous.curidx = 1;
      Slideous.importproperties();
      Slideous.registerslides();
      document.body.innerHTML = Slideous.injectproperties(document.body.innerHTML);
      Slideous.buildtocs();
      Slideous.registeranchors();
      Slideous.toggleshowall(false);
      Slideous.updatestatus();
      document.body.style.fontSize = Slideous.fontsize+"%";
      document.getElementById("s1").style.display = "block";
      document.onkeydown = Slideous.keydown;
      Slideous.mousenavigation(Slideous.mousesensitive);
      Slideous.registerbuttons();
      if (window.location.hash)
         Slideous.gotoslide(window.location.hash.substr(2), true, true);
   },
   registerslides: function() {
      var div = document.getElementsByTagName("div");
      Slideous.count = 0;
      for (var i in div)
         if (Slideous.hasclass(div[i], "slide"))
            div[i].setAttribute("id", "s"+(++Slideous.count));
   },
   registeranchors: function() {
      var a = document.getElementsByTagName("a"),
          loc = (window.location.hostname+window.location.pathname).replace(/\\/g, "/");
      for (var i in a) {
         if (a[i].href && a[i].href.indexOf(loc) >= 0 && a[i].href.lastIndexOf("#") >= 0) {
            a[i].href = "javascript:Slideous.gotoslide(" + a[i].href.substr(a[i].href.lastIndexOf("#")+2)+",true,true)";
         }
      }
   },
   registerbuttons: function() {
      var button;
      for (var b in Slideous.navbuttons)
         if (button = document.getElementById(b))
            button.onclick = Slideous.navbuttons[b];
   },
   importproperties: function() {  // from html meta section ..
      var meta = document.getElementsByTagName("meta"), elem;
      for (var i in meta)
         if (meta[i].attributes && meta[i].attributes["name"] && meta[i].attributes["name"].value in Slideous)
            switch (typeof(Slideous[meta[i].attributes["name"].value])) {
               case "number": Slideous[meta[i].attributes["name"].value] = parseInt(meta[i].attributes["content"].value); break;
               case "boolean": Slideous[meta[i].attributes["name"].value] = meta[i].attributes["content"].value == "true" ? true : false; break;
               default: Slideous[meta[i].attributes["name"].value] = meta[i].attributes["content"].value; break;
            }
   },
   injectproperties: function(str) {
      var meta = document.getElementsByTagName("meta"), elem;
      for (var i in meta) {
         if (meta[i].attributes && meta[i].attributes["name"])
            str = str.replace(new RegExp("{\\$"+meta[i].attributes["name"].value+"}","g"), meta[i].attributes["content"].value);
      }
      return str = str.replace(/{\$generator}/g, "Slideous")
                      .replace(/{\$version}/g, Slideous.version)
                      .replace(/{\$title}/g, document.title)
                      .replace(/{\$slidecount}/g, Slideous.count);
   },
   buildtocs: function() {
      var toc = document.getElementById("toc"), list = "",
          tocbox = document.getElementById("tocbox");
      if (toc) {
         for (var i=0; i<Slideous.count; i++)
            list += Slideous.tocitems.toc.replace(/{\$slideidx}/g, i+1).replace(/{\$slidetitle}/, document.getElementById("s"+(i+1)).getElementsByTagName("h1")[0].innerHTML);
         toc.innerHTML = list;
         while (toc && !Slideous.hasclass(toc, "slide")) toc = toc.parentNode;
         if (toc) Slideous.tocidx = toc.getAttribute("id").substr(1);
      }
      if (tocbox) {
         tocbox.innerHTML = "";
         for (var i=0; i<Slideous.count; i++)
            tocbox.options[tocbox.length] = new Option((i+1)+". "+document.getElementById("s"+(i+1)).getElementsByTagName("h1")[0].innerHTML, "#s"+(i+1));
         tocbox.onchange = function() { Slideous.gotoslide(this.selectedIndex+1, true, true); };
      }
   },
   next: function(deep) {
      if (!Slideous.showall) {
         var slide = document.getElementById("s"+Slideous.curidx),
             item = Slideous.firstitem(slide, Slideous.isitemhidden);
         if (deep) {  // next item
            if (item)
               Slideous.displayitem(item, true);
            else
               Slideous.gotoslide(Slideous.curidx+1, false, false);
         }
         else if (item)  // complete slide ..
            while (item = Slideous.firstitem(slide, Slideous.isitemhidden))
               Slideous.displayitem(item, true);
         else           // next slide
            Slideous.gotoslide(Slideous.curidx+1, true, false);
         Slideous.updatestatus();
      }
   },
   previous: function(deep) {
      if (!Slideous.showall) {
         var slide = document.getElementById("s"+Slideous.curidx);
         if (deep) {
            var item = Slideous.lastitem(slide, Slideous.isitemvisible);
            if (item)
               Slideous.displayitem(item, false);
            else
               Slideous.gotoslide(Slideous.curidx-1, true, false);
         }
         else
            Slideous.gotoslide(Slideous.curidx-1, true, false);
         Slideous.updatestatus();
      }
   },
   gotoslide: function(i, showitems, updatestatus) {
      if (!Slideous.showall && i > 0 && i <= Slideous.count && i != Slideous.curidx) {
         document.getElementById("s"+Slideous.curidx).style.display = "none";
         var slide = document.getElementById("s"+(Slideous.curidx=i)), item;
         while (item = Slideous.firstitem(slide, showitems ? Slideous.isitemhidden : Slideous.isitemvisible))
            Slideous.displayitem(item, showitems);
         slide.style.display = "block";
         if (updatestatus)
            Slideous.updatestatus();
      }
   },
   firstitem: function(root, filter) {
      var found = filter(root);
      for (var node=root.firstChild; node!=null && !found; node = node.nextSibling)
         found = Slideous.firstitem(node, filter);
      return found;
   },
   lastitem: function(root, filter) {
      var found = null;
      for (var node=root.lastChild; node!=null && !found; node = node.previousSibling)
         found = Slideous.lastitem(node, filter);
      return found || filter(root);
   },
   isitem: function(node, visible) {
      var nodename;
      return node && node.nodeType == 1   // elements only ..
          && (nodename=node.nodeName.toLowerCase()) in Slideous.incrementables
          && (   Slideous.incrementables[nodename].filter.match("\\bself\\b") && (Slideous.hasclass(node, "incremental") || (Slideous.autoincrement && nodename in Slideous.autoincrementables))
              || Slideous.incrementables[nodename].filter.match("\\bparent\\b") && (Slideous.hasclass(node.parentNode, "incremental") || (Slideous.autoincrement && node.parentNode.nodeName.toLowerCase() in Slideous.autoincrementables))
              || Slideous.incrementables[nodename].filter.match("\\bgrandparent\\b") && (Slideous.hasclass(node.parentNode.parentNode, "incremental") || (Slideous.autoincrement && node.parentNode.parentNode.nodeName.toLowerCase() in Slideous.autoincrementables))
             )
          && (visible ? (node.style.visibility != "hidden")
                      : (node.style.visibility == "hidden"))
          ? node : null;
   },
   isitemvisible: function(node) { return Slideous.isitem(node, true); },
   isitemhidden: function(node) { return Slideous.isitem(node, false); },
   displayitem: function(item, show) {
      if (item) item.style.visibility = (show ? "visible" : "hidden");
   },
   updatestatus: function() {
      if (Slideous.statusbar) {
         var eos = document.getElementById("eos"), 
             idx = document.getElementById("slideidx"),
             tocbox = document.getElementById("tocbox");
         if (eos) 
            eos.style.visibility = Slideous.firstitem(document.getElementById("s"+Slideous.curidx), Slideous.isitemhidden) != null
                                 ? "visible" : "hidden";
         if (idx) 
            idx.innerHTML = Slideous.curidx;
         if (tocbox)
            tocbox.selectedIndex = Slideous.curidx-1;
      }
   },
   changefontsize: function(delta) {
      document.body.style.fontSize = (Slideous.fontsize+=delta)+"%";
   },
   togglestatusbar: function() {
      document.getElementById("statusbar").style.display = (Slideous.statusbar = !Slideous.statusbar) ? "block" : "none";
   },
   toggleshowall: function(showall) {
      var slide, item;
      for (var i=0; i<Slideous.count; i++) {
         slide = document.getElementById("s"+(i+1));
         slide.style.display = showall ? "block" : "none";
         while (item = Slideous.firstitem(slide, showall ? Slideous.isitemhidden : Slideous.isitemvisible)) 
            Slideous.displayitem(item, showall);
         var divs = slide.getElementsByTagName("div");
         for (var j in divs)
            if (Slideous.hasclass(divs[j], "handout"))
               divs[j].style.display = showall ? "block" : "none";
      }
      if (!showall)
         document.getElementById("s"+Slideous.curidx).style.display = "block";
      if (Slideous.statusbar) 
         document.getElementById("statusbar").style.display = showall ? "none" : "block";
      Slideous.showall = showall;
   },
   hasclass: function(elem, classname) {
      var classattr = null;
      return (classattr=(elem.attributes && elem.attributes["class"])) 
          && classattr.nodeValue.match("\\b"+classname+"\\b");
   },
   selectedcontent: function() {
      return window.getSelection ? window.getSelection().toString() 
                                 : document.getSelection ? document.getSelection() 
                                                         : document.selection ? document.selection.createRange().text
                                                                              : "";
   },
   mousenavigation: function(on) {
      if (on) {
         document.onmousedown = Slideous.mousedown;
         document.onmouseup = Slideous.mouseup;
      }
      else
         document.onmousedown = document.onmouseup = null;
   },
   mousepos: function(e) {
      return e.pageX ? {x: e.pageX, y: e.pageY} 
                     : {x: e.x+document.body.scrollLeft, y: e.y+document.body.scrollTop};
   },
   mousedown: function(evt) {
      evt = evt||window.event;
      Slideous.mousedownpos = Slideous.mousepos(evt);
      Slideous.contentselected = !!Slideous.selectedcontent() || ((evt.target || evt.srcElement).nodeName.toLowerCase() in Slideous.clickables);
      return true;
   },
   mouseup: function(evt) {
      evt = evt||window.event;
      var pos = Slideous.mousepos(evt);
      if (pos.x == Slideous.mousedownpos.x && pos.y == Slideous.mousedownpos.y && !Slideous.contentselected) {
         Slideous.next(true);
         return evt.returnValue = !(evt.cancel = true);
      }
      return false;
   }
};
window.onload = Slideous.init;
