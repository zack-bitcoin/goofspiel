var links = (function(){

    function freeplay(){
        link = document.createElement("a");
        link.href = "open_offers.html";
        link.innerHTML = "find someone to play with";
        return(link);
    };
    function user(aid){
        link = document.createElement("a");
        link.href = "user.html?uid=".concat(aid);
        link.innerHTML = "user info";
        return(link);
    };
    function game(gid){
        link = document.createElement("a");
        link.href = "game.html?gid=".concat(gid);
        link.innerHTML = "game";
        return(link);
    };
    function history(gid){
        link = document.createElement("a");
        link.href = "history.html?gid=".concat(gid);
        link.innerHTML = "game";
        return(link);
    };

    return ({
        freeplay: freeplay,
        user: user,
        game: game,
        history: history
    });
})();
