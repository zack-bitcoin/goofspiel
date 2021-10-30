(function(){
    var div = document.createElement("div");
    document.body.appendChild(div);
    var username_input = text_input("username: ", div);
    div.appendChild(br());
    div.appendChild(br());
    var password_input = text_input("password: ", div);
    var button = button_maker2("make account", make_account);
    div.appendChild(br());
    div.appendChild(br());
    div.appendChild(button);
    div.appendChild(br());
    div.appendChild(br());

    async function make_account(){
        var name = username_input.value.trim();
        var password = password_input.value.trim();
        var key = keys.new_keys_entropy(name + "|" + password);
        keys.set_keys(key);
        var tx = ["create_account", keys.pub(),
                  2, btoa(name), 0];
        var stx = keys.sign(tx);
        var aid = await rpc.apost(
            ["create_account", stx]);
        button.onlick = "";
        var priv = key.getPrivate("hex");
        localStorage.setItem("goofspiel", JSON.stringify([priv, aid]));
        //window.open(links.user(aid), "_self");
        window.open("user.html", "_self");
    };
})();
