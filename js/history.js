(async function(){
    var div = document.createElement("div");
    document.body.appendChild(div);

    const urlParams = new URLSearchParams(window.location.search);
    var gid = urlParams.get('gid');
    gid = gid.replace(/\ /g, "+");
    gid = parseInt(gid);
    console.log(gid);

    //look up this game from history
//-record(game, {hands, player1, player2, date, title = "", timer, result}).
    var game = await rpc.apost(["read", 5, gid]);
    console.log(JSON.stringify(game));

    var hands = game[1].slice(1);
    var player1 = game[2];
    var player2 = game[3];
    var date = game[4];
    var title = atob(game[5]);
    var timer = game[6];
    var result = game[7];

    var user1 = await rpc.apost(
        ["read", 9, player1]);
    var user2 = await rpc.apost(
        ["read", 9, player2]);
    
    var p = document.createElement("p");
    div.appendChild(p);
    var result_text;
    if(result === 1){
        result_text = "player 1 winning";
    } else if(result === 2){
        result_text = "player 2 winning";
    } else if(result === 3){
        result_text = "a tie";
    };
        

    p.innerHTML = ""
        .concat("game title: ")
        .concat(title)
        .concat("<br> player1: ")
        .concat(atob(user1))
        .concat(" id: ")
        .concat(player1)
        .concat("<br>player2: ")
        .concat(atob(user2))
        .concat(" id: ")
        .concat(player2)
        .concat("<br> the game resulted in ")
        .concat(result_text);
    
})();
