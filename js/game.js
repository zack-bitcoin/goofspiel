(async function(){
    var div = document.createElement("div");
    document.body.appendChild(div);

    var aid = load_keys();

    const urlParams = new URLSearchParams(window.location.search);
    var gid = urlParams.get('gid');
    gid = gid.replace(/\ /g, "+");
    gid = parseInt(gid);
    console.log(gid);

    var round_board = document.createElement("div");
    div.appendChild(round_board);

    await draw();

    await cron(0);

    async function cron(hands){
        var x = await rpc.apost(["read", 6, gid, hands]);
        console.log("cron hands");
        console.log(x);
        if(!(x === 0)){
            draw();
            return(cron(hands + 1));
        } else {
            return(cron(hands));
        };
    };
    async function draw(){

        var temp_div = document.createElement("div");
        // look up the game board
        console.log("lookup");
        var board = await rpc.apost(["read", 6, gid, -1]);
        console.log(board);
        if((board === 0)||(board === -1)){
        //if(false){
            console.log("game ended");
            window.open("history.html?gid=".concat(gid), "_self");
        };
        console.log("lookup2");
        //{"game", hands, player1, player2, date,
        // title, card1, card2, prize, time_span,
        // timer}
        var hands = board[1].slice(1);
        var player1 = board[2];
        var player2 = board[3];

        var [points1, points2] =
            calculate_points(hands, 0, 0);
        
        var user1 = await rpc.apost(
            ["read", 9, player1]);
        var user2 = await rpc.apost(
            ["read", 9, player2]);
        
        var title = board[4];
        var card1 = board[6];
        var card2 = board[7];
        var prize = board[8];
        
        console.log(JSON.stringify(board));
        var info = document.createElement("p");
        info.innerHTML = "player1: "
            .concat(atob(user1))
            .concat(" id: ")
            .concat(player1)
            .concat("<br>player2: ")
            .concat(atob(user2))
            .concat(" id: ")
            .concat(player2)
            .concat("<br><br>prize for this round: ")
            .concat(prize)
        ;
        temp_div.appendChild(info);
        
        
        var your_turn = true;
        if(aid === player2){
            your_turn = (card2 === 0);
        }else if(aid === player1){
            your_turn = (card1 === 0);
        }
        
        var rounds = hands;
        console.log(JSON.stringify(rounds));
        var hand1 = range(1, 13);
        var hand2 = range(1, 13);
        var prizes = range(1, 13);
        [hand1, hand2, prizes] =
            remove_used(rounds,
                        [hand1, hand2, prizes]);
        if(aid == player2){
            var temp;

            temp = points1;
            points1 = points2;
            points2 = temp;

            temp = hand1;
            hand1 = hand2;
            hand2 = temp;
        }
        
        var p = document.createElement("p");
        p.innerHTML =
            "your points: "
            .concat(points1.toString())
            .concat("<br>their points: ")
            .concat(points2.toString())
            .concat("<br>their cards: ")
            .concat(ints_to_cards_string(hand2))
            .concat("<br> prize cards: ")
            .concat(ints_to_cards_string(prizes));
        temp_div.appendChild(p);
        
        if(your_turn){
            //draw pictures of your cards.
            var play_msg = document.createElement("spam");
            play_msg.innerHTML = "choose card to play: ";
            temp_div.appendChild(play_msg);
            draw_cards(hand1, temp_div);
            
        } else {
            var p = document.createElement("p");
            p.innerHTML = "waiting for them to choose their card";
            temp_div.appendChild(p);
        };
        round_board.innerHTML = "";
        round_board.appendChild(temp_div);

        console.log("lookup next\n");
        //await rpc.apost(["read", 6, gid, (hands).length]);
        //console.log("returned next\n");
        //draw();
        //window.open(links.game(gid), "_self");
        //return(0);
    };
    function draw_cards(cards, div){
        if(cards.length === 0){
            return(0);
        };
        var card = cards[0];
        var button = button_maker2(
            card.toString(), async function(){
                console.log("clicked button".concat(card.toString()));
                //freezing on the nonce request.
                var nonce = await rpc.apost(
                    ["nonce", aid]);
                console.log("got account nonce ".concat(nonce.toString()));
                var tx = ["x", keys.pub(),
                          nonce + 1, 1, aid,
                          gid, card];
                console.log(JSON.stringify(tx));
                var stx = keys.sign(tx);
                //var advance =
                await rpc.apost(["x", 1, stx]);
                console.log("published tx");
                //console.log(advance);
                await draw();
            });
        div.appendChild(button);
        //div.appendChild(br());
        //div.appendChild(br());

        return(draw_cards(cards.slice(1), div));
    };
    function ints_to_cards_string(s) {
        if(s.length === 0){
            return("");
        };
        var r = (s[0]);
        if(s.length > 1){
            return(r.toString().concat(", ").concat(
                ints_to_cards_string(s.slice(1))));
        };
        return(r);
    };
    function remove_used(l, s) {
        if(l.length === 0){
            return(s);
        };
        //console.log(l[0]);
        var [a1, a2, a3] = l[0].slice(1);
        var [h1l, h2l, prizesl] = s;
        s2 = [remove(h1l, a1),
              remove(h2l, a2),
              remove(prizesl, a3)];
        return(remove_used(l.slice(1), s2));
    }
    function remove(l, x){
        //console.log(JSON.stringify([x, l]));
        if(l.length === 0){
            return([]);
        };
        if(l[0] === x){
            return(l.slice(1));
        };
        return([l[0]].concat(remove(
            l.slice(1), x)));
    };
    function range(a, b){
        if(a === b){
            return([a]);
        };
        if(a < b){
            return([a].concat(range(a+1, b)));
        }
    };
    function calculate_points(l, p1, p2){
        if(l.length === 0){
            return([p1, p2]);
        };
        [c1, c2, points] = l[0].slice(1);
        var r = l.slice(1);
        if(c1 > c2){
            return(calculate_points(
                r, p1 + points, p2));
        } else if(c2 > c1){
            return(calculate_points(
                r, p1, p2 + points));
        } else {
            return(calculate_points(
                r, p1, p2));
        };
    };
})();
