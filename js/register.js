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
    var link = document.createElement("div");
    div.appendChild(link);

    async function make_account(){
        var name = username_input.value.trim();
        var password = password_input.value.trim();
        var key = keys.new_keys_entropy(name + "|" + password);
        keys.set_keys(key);
        var priv = key.getPrivate("hex");
        var tx = ["create_account", keys.pub(),
                  2, btoa(name)];
        var stx = keys.sign(tx);
        var aid = await rpc.apost(
            ["create_account", stx]);
        localStorage.setItem("goofspiel", JSON.stringify([priv, aid]));
        console.log(JSON.parse(localStorage.getItem("goofspiel")));
        console.log([priv, aid]);
        button.onlick = "";
        link_to_user = document.createElement("a");
        link_to_user.href = "users.html?uid=".concat(aid);
        link_to_user.innerHTML = "continue";
        link.innerHTML = "";
        link.appendChild(link_to_user);
    };
        
    
    //button to continue. The button should load the private key and account id into memory storage, then send you to the user page.
})();
