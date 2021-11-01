Goofspiel
=====

A game server to play goofspiel.
[try it out here](http://46.101.185.98:8001/introduction.html)

Uses public key cryptography for user security.
Remembers a history of games you won and loss.

turn it on:
```
sh start.sh
```

after you turn it on, the local page for users can be seen here:
http://127.0.0.1:8001/home.html

it automatically starts in the background. You can attach to it to be able to run commands like this:
```
sh attach.sh
```

After attaching, you can turn the system off and preserve the current state like this:
```
utils:off().
halt().
```

or you can detatch from the running process, and return it to the background by holding the Control key, and clicking the 'D' key.

