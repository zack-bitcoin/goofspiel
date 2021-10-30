-module(planned_games).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         make_game/5,
         store/2, read/1, remove/1]).
-define(LOC, "planned_games.db").
-record(game, {player1, player2, date, title = "", timer}).

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
    io:format("planned games died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({remove, ID}, D) -> 
    D2 = dict:erase(ID, D),
    {noreply, D2};
handle_cast({store, ID, Game}, D) -> 
    D2 = dict:store(ID, Game, D),
    {noreply, D2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, ID}, _From, X) -> 
    {reply, dict:find(ID, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

make_game(Pid1, Pid2, Date, Title, Timer) ->
    #game{player1 = Pid1,
         player2 = Pid2, date = Date,
         title = Title, timer = Timer}.

store(ID, Game) when is_record(Game, game) ->
    gen_server:cast(?MODULE, {store, ID, Game}).
read(ID) ->
    gen_server:call(?MODULE, {read, ID}).
remove(ID) ->
    gen_server:cast(?MODULE, {remove, ID}).
    
