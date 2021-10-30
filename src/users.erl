-module(users).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,

         empty_user/1,

         read/1, store/1, remove/1,
         active_to_history/2,
         new_direct_offer/3,
         cancel_direct_offer/3,
         delay_offers/3,
         delay_active/3,
         new_active_game/3,
         new_open_offer/2,
         remove_open_offer/2,
         set_message/2
]).
-define(LOC, "users.db").
-record(user, 
        {history = [], planned = [], active = [], 
         got_offers = [], 
         gave_offers = [], name = "",
         open_offers = [], message = <<>>}).
-record(db, {next = 1, users = dict:new()}).

empty_user(Name) -> #user{name = Name}.

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
    io:format("users died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({active_to_history, PID, GID}, 
            X = #db{users = D}) -> 
    case dict:find(PID, D) of
        error -> {noreply, X};
        {ok, V = #user{active = A1, 
                       history = H1}} ->
            A2 = remove_from(GID, A1),
            H2 = [GID|H1],
            V2 = V#user{history = H2,
                         active = A2},
            D2 = dict:store(PID, V2, D),
            {noreply, X#db{users = D2}}
    end;
handle_cast({remove, ID}, X = #db{users = D}) -> 
    D2 = dict:erase(ID, D),
    {noreply, X#db{users = D2}};
handle_cast({new_direct_offer, 
             From, To, GID}, 
            X = #db{users = D}
           ) -> 
    case dict:find(From, D) of
        error -> {noreply, X};
        {ok, V1} ->
            case dict:find(To, D) of
                error -> {noreply, X};
                {ok, V2} ->
                    V1b = V1#user{gave_offers = [GID|V1#user.gave_offers]},
                    V2b = V2#user{got_offers = [GID|V2#user.got_offers]},
                    D2 = dict:store(From, V1b, D),
                    D3 = dict:store(To, V2b, D2),
                    {noreply, X#db{users = D3}}
            end
    end;
handle_cast({cancel_direct_offer, 
             From, To, GID}, 
            X = #db{users = D}
           ) -> 
    case dict:find(From, D) of
        error -> {noreply, X};
        {ok, V1} ->
            case dict:find(To, D) of
                error -> {noreply, X};
                {ok, V2} ->
                    V1b = V1#user{gave_offers = remove_from(GID, V1#user.gave_offers)},
                    V2b = V2#user{got_offers = remove_from(GID, V2#user.got_offers)},
                    D2 = dict:store(From, V1b, D),
                    D3 = dict:store(To, V2b, D2),
                    {noreply, X#db{users = D3}}
            end
    end;
handle_cast({new_active_game, P1, P2, GID}, 
            X = #db{users = D}) -> 
    case dict:find(P1, D) of
        error -> {noreply, X};
        {ok, V1} ->
            case dict:find(P2, D) of
                error -> {noreply, X};
                {ok, V2} ->
                    V1b = V1#user{active = [GID|V1#user.active]},
                    V2b = V2#user{active = [GID|V2#user.got_offers]},
                    D2 = dict:store(P1, V1b, D),
                    D3 = dict:store(P2, V2b, D2),
                    {noreply, X#db{users = D3}}
            end
    end;
handle_cast({new_open_offer, P1, GID}, 
            X = #db{users = D}) -> 
    case dict:find(P1, D) of
        error -> {noreply, X};
        {ok, V1} ->
            V1b = V1#user{open_offers = [GID|V1#user.open_offers]},
            D2 = dict:store(P1, V1b, D),
            {noreply, X#db{users = D2}}
    end;
handle_cast({remove_open_offer, P1, GID}, 
            X = #db{users = D}) -> 
    case dict:find(P1, D) of
        error -> {noreply, X};
        {ok, V1} ->
            V1b = V1#user{open_offers = remove_from(GID, V1#user.open_offers)},
            D2 = dict:store(P1, V1b, D),
            {noreply, X#db{users = D2}}
    end;
handle_cast({set_message, P1, Msg}, 
            X = #db{users = D}) -> 
    case dict:find(P1, D) of
        error -> {noreply, X};
        {ok, V1} ->
            V1b = V1#user{message = Msg},
            D2 = dict:store(P1, V1b, D),
            {noreply, X#db{users = D2}}
    end;

handle_cast(_, X) -> {noreply, X}.
handle_call({store, User}, 
            _From, 
            X = #db{next = ID, 
                    users = Users}
           ) -> 
    Users2 = dict:store(ID, User, Users),
    {reply, ID, X#db{next = ID + 1,
                       users = Users2}};
handle_call({read, ID}, _From, 
            X = #db{users = D}) -> 
    {reply, dict:find(ID, D), X};
handle_call(_, _From, X) -> {reply, X, X}.

read(X) ->
    gen_server:call(?MODULE, {read, X}).
store(User) when is_record(User, user) ->
    gen_server:call(?MODULE, {store, User});
store(Name) -> store(empty_user(Name)).
remove(X) ->
    gen_server:cast(?MODULE, {remove, X}).
active_to_history(PID, GID) ->
    true = is_integer(PID),
    true = is_integer(GID),
    true = (PID > 0),
    true = (GID > 0),
    gen_server:cast(
      ?MODULE, {active_to_history, PID, GID}).
new_direct_offer(From, To, GID) ->
    true = is_integer(From),
    true = is_integer(To),
    true = is_integer(GID),
    true = (From > 0),
    true = (To > 0),
    true = (GID > 0),
    gen_server:cast(
      ?MODULE, {new_direct_offer, From, To, GID}).
cancel_direct_offer(From, To, GID) ->
    true = is_integer(From),
    true = is_integer(To),
    true = is_integer(GID),
    true = (From > 0),
    true = (To > 0),
    true = (GID > 0),
    gen_server:cast(
      ?MODULE, {cancel_direct_offer, From, To, GID}).
new_active_game(P1, P2, GID) ->
    true = is_integer(P1),
    true = is_integer(P2),
    true = is_integer(GID),
    true = (P1 > 0),
    true = (P2 > 0),
    true = (GID > 0),
    gen_server:cast(
      ?MODULE, {new_active_game, P1, P2, GID}).
new_open_offer(P1, GID) ->
    true = is_integer(P1),
    true = is_integer(GID),
    true = (P1 > 0),
    true = (GID > 0),
    gen_server:cast(
      ?MODULE, {new_open_game, P1, GID}).
remove_open_offer(P1, GID) ->
    true = is_integer(P1),
    true = is_integer(GID),
    true = (P1 > 0),
    true = (GID > 0),
    gen_server:cast(
      ?MODULE, {remove_open_game, P1, GID}).
set_message(P1, Msg) ->
    true = is_integer(P1),
    true = is_binary(Msg),
    true = (size(Msg) < 140),
    gen_server:cast(
      ?MODULE, {set_message, P1, Msg}).
    

%it responds if you receive a new direct offer, otherwise it waits.
delay_offers(_Who, _Many, Delay) 
  when (Delay < 0) -> 0;
delay_offers(Who, Many, Delay) ->
    case read(Who) of
        error -> ok;
        {ok, #user{got_offers = L}} ->
            B = (length(L) == Many),
            if
                not(B) -> L;
                true ->
                    timer:sleep(1000),
                    delay_offers(Who, Many, Delay-10)
            end
    end.
%it responds if a new game starts, otherwise it waits.
delay_active(_Who, _Many, Delay)
  when (Delay < 0) -> 0;
delay_active(Who, Many, Delay) ->
    case read(Who) of
        error -> ok;
        {ok, #user{active = L}} ->
            B = (length(L) == Many),
            if
                not(B) -> L;
                true ->
                    timer:sleep(1000),
                    delay_offers(
                      Who, Many, Delay-10)
            end
end.
     

remove_from(_, []) -> [];
remove_from(X, [X|T]) -> T;
remove_from(X, [A|T]) -> 
    [A|remove_from(X, T)].
