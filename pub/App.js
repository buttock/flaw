var App = (function () {

    var pub = {};

    var home;
    pub.getHome = function() { return home; }

    var cfg;
    pub.setCfg = function(c) { cfg = c; }
    pub.getCfg = function() { return cfg; }

    function eventsUrl() {
        return cfg.url + "/events";
    }

    function nextEventsUrl(nEvents) {
        return eventsUrl() + "/" + nEvents;
    }

    var eventHandler = log;
    pub.setEventHandler = function (f) { eventHandler = f; }

    var events = [];

    function processEvents(eventsText, nEvents) {
        var ee = JSON.parse(eventsText);
        each(ee, function (e) {
            events.push(e);
            eventHandler(e);
        });
    }

    function listenEvents() {
        log("listenEvents");
        var nEvents = events.length;
        http({ method: "GET"
             , url: nextEventsUrl(nEvents)
             , success: function (eventsText) {
                            processEvents(eventsText, nEvents);
                            listenEvents();
                        }
             , failure: function (status) {
                            warn("Couldn't retrieve game events (status " + status + ").");
                        }
             });
    }

    pub.send = function (eventsData, success, failure) {
        return http({ method: "POST"
                    , url: eventsUrl()
                    , contentType: "application/json; charset=UTF-8"
                    , data: eventsData
                    , success: success || nop
                    , failure: failure || function () { warn("SEND EVENT FAILED: " + eventData); }
                    });
    }

    var start = nop;
    pub.setStart = function (f) { start = f; }

    pub.onload = function() {
        log("startGame");
        home = eltById(cfg.gameHomeId);
        start();
        listenEvents();
    }

    return pub;
})();

