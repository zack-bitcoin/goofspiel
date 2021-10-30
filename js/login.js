(function(){
    var div = document.createElement("div");
    document.body.appendChild(div);
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
