
function nop() {}
var log = typeof(console) == 'undefined' ? nop : console.log;
var warn = typeof(console) == 'undefined' ? nop : console.warn;

function onloadHandler () {}

function configGame(cfg) {
    log("configGame");
    onloadHandler = startGame(cfg);
}

function startGame(cfg) { return function () {
    log("startGame");

    /*
     * User Interface
     */

    var gameHome = eltById(cfg.gameHomeId);

    gameHome.appendChild(div({},
        actionButton("dealCard", "Deal Card", eventsUrl()
                    , postEvent
                    , eventDataMaker(dealCardEvent))));

    
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
            handleEvent(e);
        });
    }

    function handleEvent(e) {
        // just print the event out on the screen
        prepend(gameEvents, div({}, text(e)));
    }

    function nextEventsUrl(nEvents) {
        return eventsUrl() + "/" + nEvents;
    }

    function eventsUrl() {
        return cfg.url + "/events";
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

    function postEvent(e) {
        log("postEvent");
    }

    listenEvents();
}; }

/*
 * Event
 */

function dealCardEvent() { return "DEAL CARD"; }

function eventsDataMaker(eventMakers) {
    return function () {
        return netstring(showJSON(map(eventMakers, apply)));
    };
}

function readJSON(s) { return JSON.parse(s); }
function showJSON(v) { return JSON.stringify(v); }

var eventDataMaker = compose(eventsDataMaker, listof);

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

