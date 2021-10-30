-module(usernames).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        store/2, read/1, remove/1]).
%from name to id.
-define(LOC, "usernames.db").
-record(db, {n2i = dict:new(), 
             i2n = dict:new()}).
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
    io:format("usernames died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, Name, ID}, 
            #db{i2n = I2N, n2i = N2I}) -> 
    I2N2 = dict:store(ID, Name, I2N),
    N2I2 = dict:store(Name, ID, N2I),
    {noreply, #db{i2n = I2N2, n2i = N2I2}};
handle_cast({remove, ID}, 
            #db{i2n = I2N, n2i = N2I}) -> 
    Name = dict:find(ID, I2N),
    I2N2 = dict:erase(ID, I2N),
    N2I2 = dict:erase(Name, N2I),
    {noreply, #db{i2n = I2N2, n2i = N2I2}};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, ID}, _From, 
            D = #db{i2n = X}) -> 
    {reply, dict:find(ID, X), D};
handle_call({read_name, Name}, _From, 
            D = #db{n2i = X}) -> 
    {reply, dict:find(Name, X), D};
handle_call(_, _From, X) -> {reply, X, X}.

store(Name, ID) ->
    gen_server:cast(?MODULE, {store, Name, ID}).
read(ID) when is_integer(ID) ->
    gen_server:call(?MODULE, {read, ID});
read(Name) ->
    gen_server:call(?MODULE, {read_name, Name}).

remove(ID) when is_integer(ID) ->
    gen_server:cast(?MODULE, {remove, ID}).

