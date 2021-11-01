(function(){
    var div = document.createElement("div");
    document.body.appendChild(div);

    var aid = load_keys();

    link_to_user = links.user(aid);
    link_to_user.innerHTML = "home";

    div.appendChild(link_to_user);
    div.appendChild(br());
    div.appendChild(br());

    var game_name_input = text_input("new game name: ", div);
    var time_limit_input = text_input("time per turn: ", div);
    var button = button_maker2("make new offer", make_offer);
    div.appendChild(br());
    div.appendChild(br());
    div.appendChild(button);
    div.appendChild(br());
    div.appendChild(br());

    var offer_list = document.createElement("div");
    div.appendChild(offer_list);

    game_launcher(aid);
    
    refresh_list();

    async function make_offer(){
        var name = game_name_input.value.trim();
        var timer =
            parseFloat(time_limit_input.value);
        timer = Math.round(timer * 10);
        timer = Math.min(timer, 1200);
        timer = Math.max(timer, 50);
        var nonce = await rpc.apost(["nonce", aid]);
        console.log(aid);
        console.log(nonce);
        var tx = ["x", keys.pub(), nonce + 1, 6,
                  aid, btoa(name), timer];
        var stx = keys.sign(tx);
        console.log(JSON.stringify(stx));
        await rpc.apost(["x", 6, stx]);//publish open offer
        time_limit_input.value = "";
        game_name_input.value = "";
        refresh_list();
    };
    async function refresh_list(){
        //["read", 7]//all open offers.
        //["read", 1, gid] // one open offer
        var l = await rpc.apost(["read", 7]);
        console.log(JSON.stringify(l));
        var d = document.createElement("div");
        refresh_list2(l.slice(1), d);
    };
    function refresh_list2(l, d) {
        if(l.length === 0){
            offer_list.innerHTML = "";
            offer_list.appendChild(d);
            return(0);
        };
        var gid = l[0][1];
        var game = l[0][2];
        //var userid = game[1];
        var aid2 = game[1];
        var title = atob(game[2]);
        var timeout = game[3];

        
        var p = document.createElement("span");
        p.innerHTML = "title: "
            .concat(title)
            .concat(". time per round: ")
            .concat((timeout/10).toFixed(1))
            .concat(" seconds");

        var b;
        if(aid2 === aid){
            var b = button_maker2("cancel", async function(){
                var nonce = await rpc.apost(["nonce", aid]);
                var tx = ["x", keys.pub(),
                          nonce + 1, 7, aid, gid]
                var stx = keys.sign(tx);
                await rpc.apost(["x", 7, stx]);
                refresh_list();
            });
        } else {
            var b = button_maker2("accept", async function(){
                var nonce = await rpc.apost(["nonce", aid]);
                var tx = ["x", keys.pub(),
                          nonce + 1, 8, aid, gid];
                var stx = keys.sign(tx);
                await rpc.apost(["x", 8, stx]);
                window.open(links.game(gid), "_self");
            });
        };
        d.appendChild(p);
        d.appendChild(b);
        d.appendChild(br());
        d.appendChild(br());

        console.log(JSON.stringify(
            [aid2, gid, game, title, timeout]));
        

        refresh_list2(l.slice(1), d);
    };

    //show open offers. if it is yours, make a cancel button. otherwise, make an accept button.

})();
