function actionButton(id, name, dataMaker, success, failure) {
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
