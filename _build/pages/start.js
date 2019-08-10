function request (page) {
    var xmlhttp = new XMLHttpRequest ()
    xmlhttp.open("GET", page, false);
    xmlhttp.send(null);
    return xmlhttp.responseText;
}

function newGame () {
    var name = document.getElementById("name").value;
    document.getElementById("content").innerHTML = request ("newgame/?name=" + name);
}

function joinGame () {
    var name = document.getElementById("name").value;
    var code = document.getElementById("code").value;
    document.getElementById("content").innerHTML = request ("joingame/?name=" + name + "&game=" + code);
}
