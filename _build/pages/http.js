function request (page) {
    var xmlhttp = new XMLHttpRequest ()
    xmlhttp.open("GET", page, false);
    xmlhttp.send(null);
    return xmlhttp.responseText;
}
