-module(pubkeys).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        store/2, read/1, remove/1]).
%from id to pub.
-define(LOC, "pubkeys.erl").
-record(db, {p2i = dict:new(), i2p = dict:new()}).
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> #db{};
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("pubkeys died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, ID, Pub}, 
            X = #db{i2p = I, p2i = P}) -> 
    I2 = dict:store(ID, Pub, I),
    P2 = dict:store(Pub, ID, P),
    {noreply, X#db{i2p = I2, p2i = P2}};
handle_cast({remove, ID}, 
           X = #db{i2p = I, p2i = P}) -> 
    case dict:find(ID, I) of
        error -> {noreply, X};
        {ok, Pub} ->
            I2 = dict:erase(ID, I),
            P2 = dict:erase(Pub, P),
            {noreply, X#db{i2p = I2, p2i = P2}}
    end;
handle_cast(_, X) -> {noreply, X}.
handle_call({read, ID}, _From, 
            X = #db{i2p = I}) -> 
    {reply, dict:find(ID, I), X};
handle_call({read_pub, Pub}, _From, 
            X = #db{p2i = P}) -> 
    {reply, dict:find(Pub, P), X};
handle_call(_, _From, X) -> {reply, X, X}.

store(ID, Pub) ->
    gen_server:cast(?MODULE, {store, ID, Pub}).
read(ID) when is_integer(ID)->
    gen_server:call(?MODULE, {read, ID});
read(ID) when is_binary(ID)->
    gen_server:call(?MODULE, {read_pub, ID}).
remove(ID) ->
    gen_server:cast(?MODULE, {remove, ID}).
