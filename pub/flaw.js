
var gameUrl;
var gameHomeId;
var gameHome;
var gameEvents;
var postButton;

function onloadHandler () {}

function joinGame() {
    console.log("joinGame");
    gameHome = eltById(gameHomeId);

    postButton = input({ type: "submit"
                       , id: "postButton"
                       , name: "post"
                       , value: "Post Event"});

    var postForm = form({ method: "POST" }, postButton);
    postForm.addEventListener("submit", postEvent, true);

    gameHome.appendChild(div({}, postForm));

    gameEvents = div({});
    gameHome.appendChild(gameEvents);

    listenEvents();
}

function configGame(url, homeId) {
    console.log("configGame");
    gameUrl = url;
    gameHomeId = homeId;
    onloadHandler = joinGame;
}

var events = [];

function processEvents(eventsText, nEvents) {
    var ee = eval('(' + eventsText + ')');
    each(ee, function (e) {
        events.push(e);
        prepend(gameEvents, div({}, text(e)));
    });
}

function nextEventsUrl(nEvents) {
    return eventsUrl() + "/" + nEvents;
}

function eventsUrl() {
    return gameUrl + "/events";
}

function listenEvents() {
    console.log("listenEvents");
    var nEvents = events.length;
    http({ method: "GET"
         , url: nextEventsUrl(nEvents)
         , success: function (eventsText) {
                        processEvents(eventsText, nEvents);
                        listenEvents();
                    }
         , failure: function (status) {
                        console.warn("Couldn't retrieve game events (status " + status + ").");
                    }
         });
}

function postEvent(e) {
    console.log("postEvent");
    e.preventDefault();
    e.stopPropagation();
    disable(postButton);
    http({ method: "POST"
         , url: eventsUrl()
         , success: function () { enable(postButton); }
         , failure: function () { enable(postButton); }
         });
    return false;
}

/*
 * Ajax
 */

function http(o) { later (function() {
    var xhr = makeXHR();
    xhr.onreadystatechange = function () {
        if (this.readyState == 4) {
            if (this.status == 200) {
                o.success(this.responseText);
            }
            else {
                o.failure(this.status);
            }
            xhr.onreadystatechange = function () {}
        }
    };
    xhr.open(o.method, o.url);
    xhr.send();
}); }

function makeXHR() {
    return new window.XMLHttpRequest();
}

function later(k) { window.setTimeout(k, 0); }

/*
 * DOM
 */

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

/*
 * util
 */

function each(a,f) {
    var i, iLim;
    for (iLim=a.length, i=0; i<iLim; i++) {
        f(a[i]);
    }
}

