var AppUtils = (function () {

    var pub = {};

    /* Given a list of eventMakers (nullary functions that return strings),
     * make a function that produces an eventsData (a message representing
     * a list of events).
     */
    pub.eventsDataMaker = function(eventMakers) {
        return function () {
            return netstring(JSON.stringify(map(eventMakers, apply)));
        };
    }

    /* Given an eventMaker (a nullary function that returns a string),
     * make a function that produces an eventsData (a message representing
     * a list of events).
     */
    pub.eventDataMaker = compose(pub.eventsDataMaker, listof);

    pub.actionButton = function(id, name, dataMaker, success, failure) {
        var submitButton = input({ type: "submit"
                                 , id: id
                                 , name: "post"
                                 , value: name });
        var postForm = form({ method: "POST" }, submitButton);
        function onSubmit(e) {
            disable(submitButton);

            e.preventDefault();
            e.stopPropagation();

            App.send(dataMaker ? dataMaker(postForm) : null
                    ,function (resultText) {
                        if (success) { success(resultText); }
                        enable(submitButton);
                     }
                    ,function () {
                        if (failure) { failure(); }
                        else { alert("Action failed: " + name); }
                        enable(submitButton);
                    });
        }
        postForm.addEventListener("submit", onSubmit, true);
        return postForm;
    }

    return pub;
})();
