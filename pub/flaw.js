
function onloadHandler () {}

function configGame(cfg) {
    console.log("configGame");
    onloadHandler = startGame(cfg);
}

function startGame(cfg) { return function () {
    console.log("startGame");

    /*
     * User Interface
     */

    var gameHome = eltById(cfg.gameHomeId);

    gameHome.appendChild(div({},
        actionButton("dealCard", "Deal Card", eventsUrl()
                    , postEvent
                    , compose(netstring,
                        compose(showJSON,
                          compose(listof, dealCardEvent))))));

    var gameEvents = div({});
    gameHome.appendChild(gameEvents);

    /*
     *  Game Events
     */

    var events = [];

    function processEvents(eventsText, nEvents) {
        var ee = readJSON(eventsText);
        each(ee, function (e) {
            events.push(e);
            prepend(gameEvents, div({}, text(e)));
        });
    }

    function nextEventsUrl(nEvents) {
        return eventsUrl() + "/" + nEvents;
    }

    function eventsUrl() {
        return cfg.url + "/events";
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
    }

    listenEvents();
}; }

/*
 * Event
 */

function dealCardEvent() { return "DEAL CARD"; }

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
    if (o.data && o.contentType) {
        xhr.setRequestHeader("Content-Type", o.contentType);
    }

    xhr.send(o.data);
}); }

function makeXHR() {
    return new window.XMLHttpRequest();
}

function later(k) { window.setTimeout(k, 0); }

function readJSON(s) { return JSON.parse(s); }
function showJSON(v) { return JSON.stringify(v); }

function actionButton(id, name, url, handler, dataMaker) {
    var submitButton = input({ type: "submit"
                             , id: id
                             , name: "post"
                             , value: name });
    var postForm = form({ method: "POST" }, submitButton);
    function onSubmit(e) {
        disable(submitButton);

        e.preventDefault();
        e.stopPropagation();

        http({ method: "POST"
             , url: url
             , contentType: "application/json; charset=UTF-8"
             , data: dataMaker ? dataMaker(postForm) : null
             , success: function (resultText) {
                            handler(resultText);
                            enable(submitButton);
                        }
             , failure: function () {
                            alert("Action failed: " + name);
                            enable(submitButton);
                        }
             });
    }
    postForm.addEventListener("submit", onSubmit, true);
    return postForm;
}


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

function compose(fOuter, fInner) { return function (x) {
    return fOuter(fInner(x));
}; }

function listof(x) { return [x]; }

function netstring(s) {
    return "" + s.length + ":" + s + ",";
}

