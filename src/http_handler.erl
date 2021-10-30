-module(http_handler).
-export([init/3, handle/2, terminate/3, doit/1,
	init/2, test/0]).
%curl -i -d '[-6,"test"]' http://localhost:8000
init(Req0, Opts) ->
    handle(Req0, Opts).	
handle(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    %{IP, _} = cowboy_req:peer(Req2),
    true = is_binary(Data),
    A = packer:unpack(Data),
    B = doit(A),
    C = packer:pack(B),
    Headers = #{ 
      <<"content-type">> => 
          <<"application/octet-stream">>,
      <<"Access-Control-Allow-Origin">> => 
          <<"*">>},
    Req4 = cowboy_req:reply(
             200, Headers, C, Req2),
    {ok, Req4, State}.
init(_Type, Req, _Opts) -> 
    {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.

doit({account, ID}) -> users:read(ID);
doit({nonce, UID}) -> 
    nonces:check(UID);
doit({read, 1, GID}) ->
    open_offers:read(GID);
doit({read, 2, GID}) ->
    direct_offers:read(GID);
doit({read, 3, GID}) ->
    planned_games:read(GID);
doit({read, 4, GID}) ->
    {ok, active_games:read(GID)};
doit({read, 5, GID}) ->
    historical_games:read(GID);
doit({read, 6, GID, RoundsSeen}) ->
    %responds when the game state advances to the next round.
    Delay = 300,%in tenths of a second.
    {ok, active_games:read_round(GID, RoundsSeen, Delay)};
doit({read, 7}) ->
    open_offers:all();
doit({read, 8, Pub}) ->
    %pubkey to account id
    pubkeys:read(Pub);
doit({read, 9, AID}) ->
    %account id to username, or username to account id.
    usernames:read(AID);


%signed API
doit({x, 1, Stx}) -> 
    %play
    F = fun(Tx) ->
                {x, Pub, _, 1, PID, GID, 
                 Card} = Tx,
                {ok, Pub} = pubkeys:read(PID),
                active_games:play(GID, PID, Card)
        end,
    signed_tx(Stx, F, false);
doit({x, 2, Stx}) ->
    %make direct offer
    F = fun(Tx) ->
                {x, Pub, _, 2, P1, P2,
                Date, Title, Timer} = Tx,
                {ok, Pub} = pubkeys:read(P1),
                G = direct_offer:make_game(
                      P1, P2, Date, Title, Timer),
                GID = direct_offers:store(G),
                users:new_direct_offer(P1, P2, GID)
        end,
    signed_tx(Stx, F, false);
doit({x, 3, Stx}) ->
    %cancel direct offer
    F = fun(Tx) ->
                {x, Pub, _, 3, 
                 P1, GID} = Tx,
                {ok, Pub} = pubkeys:read(P1),
                Offer = direct_offers:read(GID),
                %verify that Pub made this offer.
                P1 = direct_offers:player1(Offer),
                P2 = direct_offers:player2(Offer),
                direct_offers:remove(GID),
                users:cancel_direct_offer(P1, P2, GID)
        end,
    signed_tx(Stx, F, false);
doit({x, 4, Stx}) ->
    %accept direct offer
    F = fun(Tx) ->
                {x, Pub2, _, 4,
                 P2, GID} = Tx,
                {ok, Pub2} = pubkeys:read(P2),
                Offer = direct_offers:read(GID),
                P1 = direct_offers:player1(Offer),
                P2 = direct_offers:player2(Offer), %check that they received this offer.
                Title = direct_offers:title(Offer),
                Date = direct_offers:date(Offer),
                TimeSpan = direct_offers:timer(Offer),
                direct_offers:remove(GID),
                active_games:new(
                  Title, P1, P2, Date, 
                  TimeSpan, GID),
                users:cancel_direct_offer(P1, P2, GID),
                users:new_active_game(P1, P2, GID)
        end,
    signed_tx(Stx, F, false);
doit({x, 5, Stx}) ->
    %delay offers
    %responds when you receive a direct offer to play a game.
    F = fun(Tx) ->
                {x, Pub, _, 5,
                 P1, N} = Tx,
                {ok, Pub} = pubkeys:read(P1),
                Delay = 300,%in tenths of a second
                users:delay_offers(P1, N, Delay)
        end,
    signed_tx(Stx, F, false);
doit({x, 50, Stx}) ->
    %delay actives
    %responds when a new active game starts
    F = fun(Tx) ->
                {x, Pub, _, 50,
                 P1, N} = Tx,
                {ok, Pub} = pubkeys:read(P1),
                Delay = 300,%in tenths of a second
                users:delay_active(P1, N, Delay)
        end,
    signed_tx(Stx, F, false);
doit({x, 6, Stx}) ->
    %make open offer
    F = fun(Tx) ->
                {x, Pub, _, 6,
                 P1, Title, TimeSpan} = Tx,
                {ok, Pub} = pubkeys:read(P1),
                G = open_offers:make_game(
                  P1, Title, TimeSpan),
                GID = open_offers:store(G),
                users:new_open_offer(P1, GID)%todo
        end,
    signed_tx(Stx, F, false);
doit({x, 7, Stx}) ->
    %cancel open offer
    F = fun(Tx) ->
                {x, Pub, _, 7,
                P1, GID} = Tx,
                {ok, Pub} = pubkeys:read(P1),
                {ok, Offer} = open_offers:read(GID),
                P1 = open_offers:player1(Offer),
                open_offers:remove(GID),
                users:remove_open_offer(P1, GID)
        end,
    signed_tx(Stx, F, false);
doit({x, 8, Stx}) ->
    %accept open offer
    F = fun(Tx) ->
                {x, Pub2, _, 8,
                 P2, GID} = Tx,
                {ok, Pub2} = pubkeys:read(P2),
                {ok, Offer} = open_offers:read(GID),
                P1 = open_offers:player1(Offer),
                TimeSpan = open_offers:timer(Offer),
                Title = open_offers:title(Offer),
                open_offers:remove(GID),
                users:remove_open_offer(P1, GID),
                Date = 0,
                active_games:new(
                  Title, P1, P2, Date, 
                  TimeSpan, GID),
                users:new_active_game(P1, P2, GID)
        end,
    signed_tx(Stx, F, false);
doit({create_account, Stx}) ->
    true = signing:verify(Stx),
    Tx = element(2, Stx),
    {create_account, Pub, _, Name, _} = Tx,
    case usernames:read(Name) of
        {ok, _} -> {ok, 0};
        error -> %name isn't used already
            UID = users:store(Name),
            pubkeys:store(UID, Pub),
            usernames:store(Name, UID),
            {ok, UID}
    end;
doit({x, 9, Stx}) ->
    %set message
    F = fun(Tx) ->
                {x, Pub, _, 9, P1, Msg} = Tx,
                {ok, Pub} = pubkeys:read(P1),
                users:set_message(P1, Msg)
        end,
    signed_tx(Stx, F, false);

doit({test}) ->
    {ok, <<"success">>}.

signed_tx(Stx, F, _AdminCheck) ->
    
    true = signing:verify(Stx),
    Tx = element(2, Stx),
    %Pub = element(2, Tx),
    ID = element(5, Tx),
    Nonce = element(3, Tx),
    {ok, PrevNonce} = nonces:check(ID),
    B = Nonce > PrevNonce,
    if
        B ->
            X = F(Tx),
            nonces:update(ID, Nonce),
            {ok, X};
        true ->
            {ok, "nonce error"}
    end.

test() ->
    {Pub, Priv} = signing:new_key(),
    {Pub2, Priv2} = signing:new_key(),
    admin:add(Pub),
    Amount = 100000,

    %inflation
    Tx = {inflate, Pub, 1, Pub2, Amount},
    Stx = signing:sign_tx(Tx, Pub, Priv),
    doit({inflate, Stx}),
   
    %spending, balance1
    {ok, Amount} = doit({balance, Pub2}),
    Tx2 = {spend, Pub2, 1, Pub, Amount div 2},
    Stx2 = signing:sign_tx(Tx2, Pub2, Priv2),
    doit({spend, Stx2}),
    A2 = Amount div 2,
    {ok, A2} = doit({balance, Pub2}),

    %new_market
    Tx3 = {new_market, Pub, 2, 
           <<"1+1=2">>, 1000, 1000},
    Stx3 = signing:sign_tx(Tx3, Pub, Priv),
    doit({new_market, Stx3}),

    %markets
    {ok, [MID]} = doit({markets}),

    %second market for browser testing
    Tx3b = {new_market, Pub, 3, 
           <<"1+1=3">>, 100, 1000},
    Stx3b = signing:sign_tx(Tx3b, Pub, Priv),
    doit({new_market, Stx3b}),

    
    %market
    {ok, {market, MID, <<"1+1=2">>, 1000, 1000}} = doit({market, MID}),

    %buy_shares, balance2
    Spend = 2500,
    Tx4 = {buy_shares, Pub2, 2, Spend, MID},
    Stx4 = signing:sign_tx(Tx4, Pub2, Priv2),
    doit({buy_shares, Stx4}),
    A3 = A2 - Spend,
    {ok, A3} = doit({balance, Pub2}),
    {ok, {Spend, Spend}} = 
        doit({balance, Pub2, MID}),

    %bet
    Tx5 = {bet, Pub2, 3, MID, 1, 2000},%the 1 means "true"
    Stx5 = signing:sign_tx(Tx5, Pub2, Priv2),
    doit({bet, Stx5}),
    {ok, {3167, 500}} = doit({balance, Pub2, MID}),
    {ok, {market, MID, _, 333, 3000}} =
        doit({market, MID}),
    
    %nonce
    {ok, 3} = doit({nonce, Pub2}),
    
    %combine_shares
    Tx6 = {combine_shares, Pub2, 4, MID},
    Stx6 = signing:sign_tx(Tx6, Pub2, Priv2),
    doit({combine_shares, Stx6}),
    {ok, {2667, 0}} = doit({balance, Pub2, MID}),
    A4 = A3 + 500,
    {ok, A4} = doit({balance, Pub2}),
    
    %oracle
    Tx7 = {oracle, Pub, 4, MID, 5000},
    Stx7 = signing:sign_tx(Tx7, Pub, Priv),
    doit({oracle, Stx7}),
    A5 = A4 + 1333,
    {ok, A5} = doit({balance, Pub2}),
        
    
    success.


