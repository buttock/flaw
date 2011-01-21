var Flaw = (function () {

    var pub = {};

    var gameEvents;

    var nDealt = 0;

    pub.start = function () {
        var home = App.getHome();
        gameEvents = div({});
        home.appendChild(div({},
            AppUtils.actionButton(
                 "dealCard"    // button id
                ,"Deal Card"   // button name
                // called when button pressed to produce eventsData:
                ,AppUtils.eventDataMaker(dealCardEvent)
                // called when event successfully posted:
                ,function(responseText) { log("SENT OK"); }
                )));
        home.appendChild(gameEvents);
    };

    pub.eventHandler = function (e) {
        // just print the event out on the screen
        log("RCVD: " + e);
        prepend(gameEvents, div({}, text(nDealt + ": " + e)));
        nDealt++;
    };

    function dealCardEvent() {
        return "DEAL CARD " + nDealt;
    }

    return pub;
})();

App.setStart(Flaw.start);
App.setEventHandler(Flaw.eventHandler);

