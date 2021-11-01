(async function(){
    var div = document.createElement("div");
    document.body.appendChild(div);

    //already logged in?
    var aid = load_keys2();
    if(aid){
        var c = document.createElement("a");
        c.href="user.html";
        var user = await rpc.apost(
            ["read", 9, aid]);
        c.innerHTML = "continue as user: ".concat(atob(user));
        div.appendChild(c);
        div.appendChild(br());
        div.appendChild(br());
    }
    var username_input = text_input("username: ", div);
    div.appendChild(br());
    div.appendChild(br());
    var password_input = text_input("password: ", div);
    var button = button_maker2("log in", login);
    div.appendChild(br());
    div.appendChild(br());
    div.appendChild(button);
    div.appendChild(br());
    div.appendChild(br());

    async function login(){
        var name = username_input.value.trim();
        var password = password_input.value.trim();
        var key = keys.new_keys_entropy(name + "|" + password);
        keys.set_keys(key);
        var aid = await rpc.apost(
            ["read", 8, keys.pub()]);
        var priv = key.getPrivate("hex");
        localStorage.setItem("goofspiel", JSON.stringify([priv, aid]));

        //window.open(links.user(aid), "_self");
        window.open("user.html", "_self");
    }
    
})();
