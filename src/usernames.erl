-module(usernames).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        store/2, read/1, remove/1]).
%from name to id.
-define(LOC, "usernames.erl").
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
    io:format("usernames died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, Name, ID}, X) -> 
    X2 = dict:store(Name, ID, X),
    {noreply, X2};
handle_cast({remove, Name}, X) -> 
    X2 = dict:erase(Name, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Name}, _From, X) -> 
    {reply, dict:find(Name, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

store(Name, ID) ->
    gen_server:cast(?MODULE, {store, Name, ID}).
read(Name) ->
    gen_server:call(?MODULE, {read, Name}).
remove(Name) ->
    gen_server:cast(?MODULE, {remove, Name}).
