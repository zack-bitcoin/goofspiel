-module(open_offers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        make_game/3,
        store/1, read/1, remove/1, all/0,
        player1/1, timer/1, title/1]).
-define(LOC, "open_offers.erl").
-record(game, {player1, title = "", timer}).

player1(G) -> G#game.player1.
timer(G) -> G#game.timer.
title(G) -> G#game.title.

init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> dict:new();
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("open offers died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({remove, ID}, D) -> 
    D2 = dict:erase(ID, D),
    {noreply, D2};
handle_cast(_, X) -> {noreply, X}.
handle_call(all, _From, D) ->
    Ks = dict:fetch_keys(D),
    Vs = lists:map(
           fun(K) -> dict:read(K, D) end,
           Ks),
    {reply, Vs, D};
handle_call({read, ID}, _From, D) ->
    {reply, dict:read(ID, D), D};
handle_call({store, Game}, _From, D) -> 
    ID = game_id:new(),
    D2 = dict:store(ID, Game, D),
    {reply, ID, D2};
handle_call(_, _From, X) -> {reply, X, X}.

make_game(Pid1, Title, Timer) ->
    #game{player1 = Pid1,
         title = Title, timer = Timer}.

store(Game) when is_record(Game, game) ->
    gen_server:call(?MODULE, {store, Game}).
read(ID) ->
    gen_server:call(?MODULE, {read, ID}).
all() ->
    gen_server:call(?MODULE, all).
remove(ID) ->
    gen_server:cast(?MODULE, {remove, ID}).
    
