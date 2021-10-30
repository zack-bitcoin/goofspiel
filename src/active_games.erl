-module(active_games).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        new/6, play/3, read/1, read_round/3,
        test/1]).
-define(LOC, "active_games.db").
-record(game, 
        {hands = [], player1, player2, date, 
         title = "", card1 = 0, card2 = 0, 
         prize, time_span, timer}).

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
    io:format("active games died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({play, GID, P, C}, X) -> 
    X2 = play_internal(GID, P, C, X),
    {noreply, X2};
handle_cast({new, GID, P1, P2, Date, TimeSpan, Title}, X) -> 
    Timer = get_now(),
    Prize = draw(range(1, 13)),
    G = #game{player1 = P1, player2 = P2, 
              date = Date, title = Title,
              prize = Prize, time_span = TimeSpan,
              timer = Timer},
    X2 = dict:store(GID, G, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, ID}, _From, X) -> 
    G = dict:find(ID, X),
    {reply, G, X};
handle_call(_, _From, X) -> {reply, X, X}.

remove_card(C, [C|T]) -> T;
remove_card(C, [X|T]) ->
    [X|remove_card(C, T)].
playable(N, Hs) ->
    %Hs = G#game.hands,
    R = range(1, 13),
    lists:foldl(fun(H, D) ->
                        C = lists:nth(N, H),
                        remove_card(C, D)
                end, R, Hs).
player_number(P, V) ->
    P1 = V#game.player1,
    P2 = V#game.player2,
    case P of
        P1 -> 1;
        P2 -> 2;
        _ -> error
    end.
play_internal(GID, P, C, X) ->
    case dict:find(GID, X) of
        error -> X;
        {ok, (V = #game{timer = Timer0,
                    time_span = Span,
                    card1 = C1,
                    card2 = C2,
                        hands = Hands})} ->
            Timer = case Hands of
                        [] -> max(1200, Timer0);%at least 2 minutes for first move.
                        _ -> Timer0
                    end,
            N = player_number(P, V),
            TimeoutCheck = expired(Timer, Span),
            AlreadyPlayedCheck = 
                case N of
                    error -> false;
                    1 -> not(C1 == 0);
                    2 -> not(C2 == 0)
                end,
            if
                TimeoutCheck ->
                    play_timeout(GID, X, V, C1, C2);
                (N == error) -> X;%unrelated account can't participate in the game.
                AlreadyPlayedCheck -> X;%you already made your move.
                true ->
                    play_internal2(
                      GID, C, X, V, N)
            end
    end.
play_internal2(GID, C, X, V, N) ->
    %check if the card they want to play is still in their hand able to be played.
    Ps = playable(N, V#game.hands),
    B = is_in(C, Ps),
    if
        B ->
            V2 = choose_card(N, C, V),
            V3 = next_hand_if_possible(GID, V2),
            case V3 of
                done -> %game ended.
                    dict:erase(GID, X);
                _ -> dict:store(GID, V3, X)
            end;
        true -> X %can't play that card.
    end.
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) -> is_in(X, T);
is_in(_, []) -> false.
    
play_timeout(GID, X, V, C1, C2) ->
    Winner = 
        if
            ((not (C1 == 0))
             and (C2 == 0)) -> 1;%player 1
            ((C1 == 0)
             and (not (C2 == 0))) -> 2;%player 2
            ((C1 == 0)
             and (C2 == 0)) -> 3%draw
        end,
    #game{
          hands = Hands,
          player1 = Player1,
          player2 = Player2,
          date = Date,
          title = Title,
          timer = Timer
         } = V,
    HGame = historical_games:make_game(
              Hands, Player1, Player2,
              Date, Title, Timer, Winner),
    end_game(GID, Player1, Player2, HGame),
    dict:erase(GID, X).
    
next_hand_if_possible(_ID, Game = #game{card1 = C1, card2 = C2}) 
  when ((C1 == 0) or (C2 == 0)) -> 
    %if someone still hasn't played, then don't go to the next round.
    Game;
next_hand_if_possible(GID, 
  Game = #game{card1 = Card1, card2 = Card2,
               prize = Prize, hands = Hands}) ->

    Hands2 = [[Card1, Card2, Prize]|Hands],
    W = winner(Hands2),
    case W of
        0 -> 
            Prize2 = draw_prize(Hands2),
            Game#game{hands = Hands2, 
                      card1 = 0, card2 = 0,
                      prize = Prize2,
                      timer = get_now()};
        _ ->%game ended.
            #game{
          player1 = Player1, player2 = Player2,
          date = Date, title = Title, 
          time_span = Timer
         } = Game,
            HGame = historical_games:make_game(
                      Hands2, Player1, Player2,
                      Date, Title, Timer, W),
            end_game(GID, Player1, Player2, HGame),
            done
    end.

end_game(GID, P1, P2, HGame) ->
    %add this to history of both users.
    %remove this from active of both users.
    users:active_to_history(P1, GID),
    users:active_to_history(P2, GID),
    historical_games:store(GID, HGame).
    

winner(Hands) ->
    %91 = 1+2+3+...+13 = 14*13/2.
    winner2(Hands, 0, 0, 91).
winner2(_, P1, _, L) when P1 > L -> 1;
winner2(_, _, P2, L) when P2 > L -> 2;
winner2(_, L, L, L) -> 3;%draw
winner2([], _, _, _) -> 0;%game hasn't ended
winner2([[C1, C2, Prize]|Hands], 
        Points1, Points2, L) ->
    if
        (C1 > C2) -> 
            winner2(Hands, Points1 + (2*Prize),
                    Points2, L);
        (C2 > C1) ->
            winner2(Hands, Points1,
                    Points2 + (2*Prize), L);
        (C1 == C2) ->
            winner2(Hands, Points1 + Prize,
                    Points2 + Prize, L)
    end.
range(N, N) -> [N];
range(N, M) when (N < M) -> 
    [N|range(N+1, M)].

draw(L) ->
    R = rand:uniform(length(L)),
    lists:nth(R, L).
draw_prize(Hs) ->    
    D = playable(3, Hs),
    draw(D).

choose_card(1, C, Game) ->
    Game#game{
      card1 = C
     };
choose_card(2, C, Game) ->
    Game#game{
      card2 = C
     }.
expired(Timer, Span) ->
    D = timer:now_diff(erlang:now(), Timer),
    B = D > (Span * 100000),%tenths of a second
    if
        B -> true;
        true -> false
    end.
            
get_now() ->
    erlang:now().



new(Title, Player1, Player2, Date, TimeSpan, GID) ->
    %todo check that the date has passed.
    %todo check that timespan is reasonable.
    true = TimeSpan > 49,%in tenths of a second
    true = TimeSpan < 1201,
    gen_server:cast(?MODULE, {new, GID, Player1, Player2, Date, TimeSpan, Title}).

play(GID, P, C) ->
    true = is_integer(C),
    true = C > 0,
    true = C < 14,
    gen_server:cast(?MODULE, {play, GID, P, C}).
read(ID) ->
    case gen_server:call(?MODULE, {read, ID}) of
        error -> 0;
        {ok, G} ->
            #game{
          card1 = C1,
          card2 = C2
         } = G,
            CA = case C1 of
                     0 -> 0;
                     _ -> 14
                 end,
            CB = case C2 of
                     0 -> 0;
                     _ -> 14
                 end,
            G#game{card1 = CA, card2 = CB}
    end.

read_round(ID, Rounds, Timeout) 
  when Timeout < 0 -> 0;
read_round(ID, Rounds, Timeout) ->
    %timeout is in tenths of a second
    G = read(ID),
    if
        (G == 0) -> -1;
        true ->
            H = G#game.hands,
            B = length(H) > Rounds,
            if
                B -> G;
                true ->
                    timer:sleep(500),
                    read_round(
                      ID, Rounds, Timeout-5)
            end
    end.


test(1) ->
    ID = 5,
    P1 = 5,
    P2 = 7,
    Date = 1234,
    TimeSpan = 51, %tenths of a second.
    new("test", P1, P2, Date, TimeSpan, ID),
    {ok, G1} = read(ID),
    play(ID, P1, 2),
    play(ID, P2, 3),
    play(ID, P1, 3),
    play(ID, P2, 3),
    {ok, G2} = read(ID),
    1 = length(G2#game.hands),%don't let them play the same card more than once.
    play(ID, P2, 4),
    play(ID, P1, 4),
    play(ID, P2, 5),
    play(ID, P1, 5),
    play(ID, P2, 6),
    play(ID, P1, 6),
    play(ID, P2, 7),
    play(ID, P1, 7),
    play(ID, P2, 8),
    play(ID, P1, 8),
    play(ID, P2, 9),
    play(ID, P1, 9),
    play(ID, P2, 10),
    play(ID, P1, 10),
    play(ID, P2, 11),
    play(ID, P1, 11),
    play(ID, P2, 12),
    play(ID, P1, 12),
    play(ID, P2, 13),
    play(ID, P1, 13),
    play(ID, P2, 1),
    play(ID, P1, 1),
    play(ID, P2, 2),
    error = read(ID),
    {ok, _} = historical_games:read(ID),
    success;
test(2) ->
    %timeout test.
    ID2 = 6,
    Date = 1234,
    TimeSpan = 51,
    P1 = 6,
    P2 = 7,
    new("test", P1, P2, Date, TimeSpan, ID2),
    play(ID2, P1, 1),
    timer:sleep(6000),
    play(ID2, P2, 2),
    error = read(ID2),
    {ok, HG} = historical_games:read(ID2),
    1 = element(8, HG),
    success.
