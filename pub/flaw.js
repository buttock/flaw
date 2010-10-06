
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
    var event = "" + nEvents;
    events.push(event);
    gameHome.appendChild(text(event));
    gameHome.appendChild(text(" "));
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

