
var gameUrl;
var gameHomeId;
var gameHome;

function onloadHandler () {}

function joinGame() {
    console.log("joinGame");
    gameHome = document.getElementById(gameHomeId);
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
        gameHome.appendChild(div({}, text(e)));
    });
}

function nextEventsUrl(nEvents) {
    return gameUrl + "/events/" + nEvents;
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
                        alert("Couldn't retrieve game events (status " + status + ").");
                    }
         });
}

function http(o) {
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
}

function makeXHR() {
    return new window.XMLHttpRequest();
}

/*
 *  DOM
 */

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

/*
function ins () {
    var container = arguments[0];
    var i, iLim;
    for (i=0, iLim=arguments.length; i<iLim; i++) {
        container.appendChild(arguments[i]);
    }
    return container;
}
*/

/*
 * util
 */

function each(a,f) {
    var i, iLim;
    for (iLim=a.length, i=0; i<iLim; i++) {
        f(a[i]);
    }
}

