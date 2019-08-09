function request (page) {
    var xmlhttp = new XMLHttpRequest ()
    xmlhttp.open("GET", page, false);
    xmlhttp.send(null);
    return xmlhttp.responseText;
}

function start () {
    document.getElementById("content").innerHTML = request ("start");
}

start();
