
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


