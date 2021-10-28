-module(direct_offers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         make_game/5,
         store/1, read/1, remove/1,
         player1/1, player2/1, title/1, 
         date/1, timer/1]).
-define(LOC, "direct_offers.erl").
-record(game, {player1, player2, date, title = "", timer}).

player1(G) -> G#game.player1.
player2(G) -> G#game.player2.
title(G) -> G#game.title.
date(G) -> G#game.date.
timer(G) -> G#game.timer.


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
    io:format("direct offers died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({remove, ID}, D) -> 
    D2 = dict:erase(ID, D),
    {noreply, D2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, ID}, _From, D) ->
    {reply, dict:read(ID, D), D};
handle_call({store, Game}, _From, D) -> 
    ID = game_id:new(),
    D2 = dict:store(ID, Game, D),
    {reply, ID, D2};
handle_call(_, _From, X) -> {reply, X, X}.

make_game(Pid1, Pid2, Date, Title, Timer) ->
    #game{player1 = Pid1,
         player2 = Pid2, date = Date,
         title = Title, timer = Timer}.

store(Game) when is_record(Game, game) ->
    gen_server:call(?MODULE, {store, Game}).
read(ID) ->
    gen_server:call(?MODULE, {read, ID}).
remove(ID) ->
    gen_server:cast(?MODULE, {remove, ID}).
    
