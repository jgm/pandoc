/*
March 19, 2004 MathHTML (c) Peter Jipsen http://www.chapman.edu/~jipsen
Released under the GNU General Public License version 2 or later.
See the GNU General Public License (at http://www.gnu.org/copyleft/gpl.html)
for more details.
*/

function convertMath(node) {// for Gecko
  if (node.nodeType==1) {
    var newnode =
      document.createElementNS("http://www.w3.org/1998/Math/MathML",
        node.nodeName.toLowerCase());
    for(var i=0; i < node.attributes.length; i++)
      newnode.setAttribute(node.attributes[i].nodeName,
        node.attributes[i].value);
    for (var i=0; i<node.childNodes.length; i++) {
      var st = node.childNodes[i].nodeValue;
      if (st==null || st.slice(0,1)!=" " && st.slice(0,1)!="\n")
        newnode.appendChild(convertMath(node.childNodes[i]));
    }
    return newnode;
  }
  else return node;
}

function convert() {
  var mmlnode = document.getElementsByTagName("math");
  var st,str,node,newnode;
  for (var i=0; i<mmlnode.length; i++)
    if (document.createElementNS!=null)
      mmlnode[i].parentNode.replaceChild(convertMath(mmlnode[i]),mmlnode[i]);
    else { // convert for IE
      str = "";
      node = mmlnode[i];
      while (node.nodeName!="/MATH") {
        st = node.nodeName.toLowerCase();
        if (st=="#text") str += node.nodeValue;
        else {
          str += (st.slice(0,1)=="/" ? "</m:"+st.slice(1) : "<m:"+st);
          if (st.slice(0,1)!="/")
             for(var j=0; j < node.attributes.length; j++)
               if (node.attributes[j].value!="italic" &&
                 node.attributes[j].value!="" &&
                 node.attributes[j].value!="inherit" &&
                 node.attributes[j].value!=undefined)
                 str += " "+node.attributes[j].nodeName+"="+
                     "\""+node.attributes[j].value+"\"";
          str += ">";
        }
        node = node.nextSibling;
        node.parentNode.removeChild(node.previousSibling);
      }
      str += "</m:math>";
      newnode = document.createElement("span");
      node.parentNode.replaceChild(newnode,node);
      newnode.innerHTML = str;
    }
}

if (document.createElementNS==null) {
  document.write("<object id=\"mathplayer\"\
  classid=\"clsid:32F66A20-7614-11D4-BD11-00104BD3F987\"></object>");
  document.write("<?import namespace=\"m\" implementation=\"#mathplayer\"?>");
}
if(typeof window.addEventListener != 'undefined'){
  window.addEventListener('load', convert, false);
}
if(typeof window.attachEvent != 'undefined') {
  window.attachEvent('onload', convert);
}
