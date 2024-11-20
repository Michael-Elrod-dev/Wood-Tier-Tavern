-module(game_fetcher).
-export([start/0, test_api/0]).

-define(API_KEY, "...").
-define(BASE_URL, "https://na1.api.riotgames.com").
-define(MAX_PLAYERS_TO_CHECK, 200).
-define(DELAY_BETWEEN_CHECKS, 500).

start() ->
    case application:ensure_all_started([jsx, inets, ssl]) of
        {ok, _} ->
            test_api();
        {error, Reason} ->
            io:format("Failed to start required applications: ~p~n", [Reason])
    end.

test_api() ->
    Url = ?BASE_URL ++ "/lol/league/v4/entries/RANKED_SOLO_5x5/IRON/IV",
    Headers = [{"X-Riot-Token", ?API_KEY}],
    
    case httpc:request(get, {Url, Headers}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            Decoded = jsx:decode(list_to_binary(Body)),
            case Decoded of
                [] -> io:format("No Iron players found~n");
                Players -> 
                    Shuffled = shuffle_list(Players),
                    check_players(Shuffled, 0)
            end;
        {ok, {{_, Code, _}, _, _}} ->
            io:format("API error getting Iron players. Status code: ~p~n", [Code]);
        Error ->
            io:format("Network error getting Iron players: ~p~n", [Error])
    end.

shuffle_list(List) ->
    Numbered = lists:zip(lists:seq(1, length(List)), List),
    Shuffled = lists:sort([{rand:uniform(), N, E} || {N, E} <- Numbered]),
    [E || {_, _, E} <- Shuffled].

check_players([], _Count) ->
    io:format("No active games found~n");
check_players(_Players, Count) when Count >= ?MAX_PLAYERS_TO_CHECK ->
    io:format("Checked ~p players, no active games found~n", [Count]);
check_players([Player|Rest], Count) ->
    io:format("\rChecking player ~p/~p...", [Count + 1, ?MAX_PLAYERS_TO_CHECK]),
    
    case check_player_game(maps:get(<<"summonerId">>, Player)) of
        {found, GameInfo} ->
            io:format("~nFound active game! Attempting to connect...~n"),
            connect_to_game(GameInfo);
        not_found ->
            timer:sleep(?DELAY_BETWEEN_CHECKS),
            check_players(Rest, Count + 1);
        {error, Reason} ->
            io:format("~nError checking player: ~p~n", [Reason]),
            check_players(Rest, Count + 1)
    end.

check_player_game(SummonerId) ->
    case get_summoner_info(SummonerId) of
        {ok, PUUID} ->
            check_active_game(PUUID);
        {error, Reason} ->
            {error, {summoner_info_failed, Reason}}
    end.

get_summoner_info(SummonerId) ->
    Url = ?BASE_URL ++ "/lol/summoner/v4/summoners/" ++ uri_string:quote(binary_to_list(SummonerId)),
    Headers = [{"X-Riot-Token", ?API_KEY}],
    
    case httpc:request(get, {Url, Headers}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            SummonerInfo = jsx:decode(list_to_binary(Body)),
            {ok, maps:get(<<"puuid">>, SummonerInfo)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {api_error, Code}};
        Error ->
            {error, {network_error, Error}}
    end.

check_active_game(PUUID) ->
    Url = ?BASE_URL ++ "/lol/spectator/v5/active-games/by-summoner/" ++ uri_string:quote(binary_to_list(PUUID)),
    Headers = [{"X-Riot-Token", ?API_KEY}],
    
    case httpc:request(get, {Url, Headers}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            try
                GameData = jsx:decode(list_to_binary(Body)),
                GameId = maps:get(<<"gameId">>, GameData),
                EncKey = maps:get(<<"encryptionKey">>, maps:get(<<"observers">>, GameData)),
                {found, {GameId, EncKey}}
            catch
                error:{badkey, Key} ->
                    {error, {missing_key, Key}};
                Error:Reason ->
                    {error, {json_parse_error, {Error, Reason}}}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            not_found;
        {ok, {{_, Code, _}, _, _}} ->
            {error, {api_error, Code}};
        Error ->
            {error, {network_error, Error}}
    end.

connect_to_game({GameId, EncryptionKey}) ->
    % Here we'll need to:
    % 1. Connect to spectator service
    % 2. Use GameId and EncryptionKey to authenticate
    % 3. Start receiving game data
    io:format("Game ID: ~p~nEncryption Key: ~p~n", [GameId, EncryptionKey]),
    % TODO: Implement actual spectator connection
    {ok, {GameId, EncryptionKey}}.