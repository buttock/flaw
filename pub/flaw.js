
var gameUrl;
var gameHome;

function joinGame(url, home) {
    gameUrl = url;
    gameHome = home;

    listenEvents();
}

var events = [];

function nextEventsUrl(nEvents) {
    return gameUrl + "/events/" + nEvents;
}

function listenEvents() {
    var nEvents = events.lenth;
    http({ method: "GET"
         , url: nextEventsUrl(nEvents)
         , success: function (eventText) {
                        events.push("" + nEvents);
                        listenEvents();
                    }
         , failure: function (status) {
                        alert("Couldn't retrieve game events.");
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

