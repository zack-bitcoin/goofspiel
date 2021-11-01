(async function(){
    var div = document.createElement("div");
    document.body.appendChild(div);

    const urlParams = new URLSearchParams(window.location.search);
    var uid = urlParams.get('uid');
    var aid = load_keys();
    if(!(uid)){
        uid = aid;
    } else {
        //load_keys();
        uid = uid.replace(/\ /g, "+");
        uid = parseInt(uid);
        console.log(uid);
    }

    var link = links.freeplay(div);
    div.appendChild(link);
    div.appendChild(br());
    div.appendChild(br());

    //var nonce = await rpc.apost(["nonce", aid]);
    var acc = await rpc.apost(["account", uid]);

    console.log(JSON.stringify(acc));
    var name = atob(acc[6]);
    var message = atob(acc[8]);
    console.log(name);
    console.log(message);

    var p = document.createElement("p");

    p.innerHTML = "userid: "
        .concat(name)
        //.concat("<br><br> about: ")
    //.concat(message)
    ;

    div.appendChild(p);

    var active = acc[3].slice(1);
    if((active.length) > 0){
        var h1 = document.createElement("h1");
        h1.innerHTML = "active games";
        div.appendChild(h1);
        list_active_games(active, div);
    };

    var history = acc[1].slice(1);
    if(history.length > 0){
        var h1 = document.createElement("h1");
        h1.innerHTML = "historical games";
        div.appendChild(h1);
        list_historical_games(history, div);
    };

    //if this user different from you, give a way to make a direct offer to this user.
    //give a link to the page that lists open offers.
    
    //display user id, username, message,
    //links to received offers
    //links to given open offer
    //links to given direct offers
    //links to planned games,
    game_launcher(aid);

    async function list_active_games(l, div){
        if(l.length === 0){
            return(0);
        };
        console.log(l[0]);
        var link = links.game(l[0]);
        link.innerHTML = "game ".concat(l[0]);
        div.appendChild(link);
        div.appendChild(br());
        div.appendChild(br());
        return(list_active_games(
            l.slice(1), div));
    };
    async function list_historical_games(l, div){
        if(l.length === 0){
            return(0);
        };
        var link = links.history(l[0]);
        link.innerHTML = "game ".concat(l[0]);
        div.appendChild(link);
        div.appendChild(br());
        return(list_historical_games(
            l.slice(1), div));
    };
    
})();
