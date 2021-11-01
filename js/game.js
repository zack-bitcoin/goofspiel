(async function(){
    var div = document.createElement("div");
    document.body.appendChild(div);

    beep();
    
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
        if(x === -1){
            console.log("game ended or never existed.");
            window.open("history.html?gid=".concat(gid), "_self");
            return(0);
        } else if(x === 0){
            console.log("cron continues");
            return(cron(hands));
        } else {
            console.log("next round was played. redraw the board.");
            draw();
            return(cron(hands + 1));
        };
    };
    async function draw(){

        var temp_div = document.createElement("div");
        // look up the game board
        var board = await rpc.apost(["read", 6, gid, -1]);
        if(board === -1){
            console.log("game ended");
            window.open("history.html?gid=".concat(gid), "_self");
            return(0);
        };
        console.log("lookup2");
        //{"game", hands, player1, player2, date,
        // title, card1, card2, prize, time_span,
        // timer}
        var hands = board[1].slice(1);
        var time_span = board[9];
        if(hands.length === 0){
            time_span = 1200;
        }
        var timer = board[10];
        var clock = make_clock(timer, time_span);
        temp_div.appendChild(clock);
        temp_div.appendChild(br());
        
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
    function clock_cron(div, starts, time_span){
        var now = Math.round(Date.now()/100);
        var elapsed = now - starts;
        var left = time_span - elapsed;
        if(left < 0){
            div.innerHTML = "out of time";
            return(0);
        }
        //console.log([starts, now]);
        div.innerHTML = (left/10).toFixed(1);
        setTimeout(function(){
            return(clock_cron(
                div, starts, time_span));
        }, 90);
    };
    function make_clock(starts, time_span){
        console.log(starts);
        starts = starts.slice(1);
        var b = starts[0] * 10000000;
        var c = starts[1] * 10;
        var e = Math.round(starts[2]/100000);
        var s = b + c + e;

        var r = document.createElement("div");
        clock_cron(r, s, time_span);
        return(r);
    };
    function beep() {
    var snd = new Audio("data:audio/wav;base64,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qJrx4KOQjahZxWbcZgztj2c49nKmkId44S71j0c8eV9yDK6uPRzx5X18eDvjvQ6yKo9ZSS6l//8elePK/Lf//IInrOF/FvDoADYAGBMGb7FtErm5MXMlmPAJQVgWta7Zx2go+8xJ0UiCb8LHHdftWyLJE0QIAIsI+UbXu67dZMjmgDGCGl1H+vpF4NSDckSIkk7Vd+sxEhBQMRU8j/12UIRhzSaUdQ+rQU5kGeFxm+hb1oh6pWWmv3uvmReDl0UnvtapVaIzo1jZbf/pD6ElLqSX+rUmOQNpJFa/r+sa4e/pBlAABoAAAAA3CUgShLdGIxsY7AUABPRrgCABdDuQ5GC7DqPQCgbbJUAoRSUj+NIEig0YfyWUho1VBBBA//uQZB4ABZx5zfMakeAAAAmwAAAAF5F3P0w9GtAAACfAAAAAwLhMDmAYWMgVEG1U0FIGCBgXBXAtfMH10000EEEEEECUBYln03TTTdNBDZopopYvrTTdNa325mImNg3TTPV9q3pmY0xoO6bv3r00y+IDGid/9aaaZTGMuj9mpu9Mpio1dXrr5HERTZSmqU36A3CumzN/9Robv/Xx4v9ijkSRSNLQhAWumap82WRSBUqXStV/YcS+XVLnSS+WLDroqArFkMEsAS+eWmrUzrO0oEmE40RlMZ5+ODIkAyKAGUwZ3mVKmcamcJnMW26MRPgUw6j+LkhyHGVGYjSUUKNpuJUQoOIAyDvEyG8S5yfK6dhZc0Tx1KI/gviKL6qvvFs1+bWtaz58uUNnryq6kt5RzOCkPWlVqVX2a/EEBUdU1KrXLf40GoiiFXK///qpoiDXrOgqDR38JB0bw7SoL+ZB9o1RCkQjQ2CBYZKd/+VJxZRRZlqSkKiws0WFxUyCwsKiMy7hUVFhIaCrNQsKkTIsLivwKKigsj8XYlwt/WKi2N4d//uQRCSAAjURNIHpMZBGYiaQPSYyAAABLAAAAAAAACWAAAAApUF/Mg+0aohSIRobBAsMlO//Kk4soosy1JSFRYWaLC4qZBYWFRGZdwqKiwkNBVmoWFSJkWFxX4FFRQWR+LsS4W/rFRb/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////VEFHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAU291bmRib3kuZGUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMjAwNGh0dHA6Ly93d3cuc291bmRib3kuZGUAAAAAAAAAACU=");  
    snd.play();
}
})();
