var Chat = (function () {

    var pub = {};

    var chatHistory;

    var msgCount = 0;

    function msgEvent(msg) { return msg; }

    pub.start = function () {
        var home = App.getHome();

        var submitButton = input({ type: "submit" , name: "post" , value: "Post" });
        var textField = input({ type: "text", name: "note", size: "30" });
        var postForm = form({ method: "POST" }
                           , paragraph({}, textField, text(" "), submitButton));

        function makeEventsData() {
            return AppUtils.mkEventsData(msgEvent(textField.value));
        }

        function onSubmit(e) {
            disable(submitButton);

            e.preventDefault();
            e.stopPropagation();

            App.send(makeEventsData()
                    ,function (resultText) {
                        textField.value = "";
                        enable(submitButton);
                     }
                    ,function (resultText) {
                        alert("Couldn't post message:" + resultText);
                        enable(submitButton);
                    });
        }
        postForm.addEventListener("submit", onSubmit, true);

        home.appendChild(postForm);

        chatHistory = div({});
        home.appendChild(chatHistory);
    };

    pub.eventHandler = function (e) {
        log("RCVD: " + e);
        prepend(chatHistory, div({}, text(msgCount + ": " + e)));
        msgCount++;
    };

    return pub;
})();

App.setStart(Chat.start);
App.setEventHandler(Chat.eventHandler);

