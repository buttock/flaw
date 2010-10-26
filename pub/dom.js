function eltById(id) { return document.getElementById(id); }

function disable(elt) { elt.setAttribute("disabled", "disabled"); }

function enable(elt) { elt.removeAttribute("disabled"); }

function text(s) { return document.createTextNode(s); }

function element(name, attrs) {
    var elt = document.createElement(name);
    for (attrName in attrs) {
        elt.setAttribute(attrName, attrs[attrName]);
    }
    return elt;
}

function eltMaker(name) { return function () {
    var attrs = arguments[0];
    var elt = element(name, attrs);
    var i, iLim;
    for (i=1, iLim=arguments.length; i<iLim; i++) {
        elt.appendChild(arguments[i]);
    }
    return elt;
}; }

var div = eltMaker("div");
var form = eltMaker("form");
var input = eltMaker("input");

function prepend(container, elt) {
    var children = container.childNodes;
    if (children.length > 0) {
        container.insertBefore(elt, children[0]);
    }
    else {
        container.appendChild(elt);
    }
}

