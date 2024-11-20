-module(game_fetcher).
-export([start/0, test_api/0]).

-define(API_KEY, get_api_key()).
-define(BASE_URL, "https://na1.api.riotgames.com").
-define(MAX_PLAYERS_TO_CHECK, 800).
-define(DELAY_BETWEEN_CHECKS, 3500).
-define(IRON_FILES, ["iron_i_players.txt", "iron_ii_players.txt", "iron_iii_players.txt", "iron_iv_players.txt"]).

get_api_key() ->
   case file:read_file("api_key.txt") of
       {ok, Binary} -> string:trim(binary_to_list(Binary));
       {error, _} -> throw({error, "API key not found in config/api_key.txt"})
   end.

start() ->
   case application:ensure_all_started([jsx, inets, ssl]) of
       {ok, _} ->
           test_api();
       {error, Reason} ->
           io:format("Failed to start required applications: ~p~n", [Reason])
   end.

read_player_file(Filename) ->
   case file:read_file(Filename) of
       {ok, Binary} ->
           Lines = string:split(Binary, "\n", all),
           [string:trim(Line) || Line <- Lines, Line /= <<>>];
       {error, Reason} ->
           io:format("Error reading ~s: ~p~n", [Filename, Reason]),
           []
   end.

test_api() ->
   AllPlayers = lists:flatten([read_player_file(File) || File <- ?IRON_FILES]),
   case AllPlayers of
       [] -> 
           io:format("No players found in files~n");
       Players -> 
           Shuffled = shuffle_list(Players),
           check_players(Shuffled, 0)
   end.

shuffle_list(List) ->
   Numbered = lists:zip(lists:seq(1, length(List)), List),
   Shuffled = lists:sort([{rand:uniform(), N, E} || {N, E} <- Numbered]),
   [E || {_, _, E} <- Shuffled].

check_players([], _Count) ->
   io:format("No active games found~n");
check_players(_Players, Count) when Count >= ?MAX_PLAYERS_TO_CHECK ->
   io:format("Checked ~p players, no active games found~n", [Count]);
check_players([PlayerId|Rest], Count) ->
   io:format("\rChecking player ~p/~p...", [Count + 1, ?MAX_PLAYERS_TO_CHECK]),
   
   case check_player_game(PlayerId) of
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
    io:format("Game ID: ~p~nEncryption Key: ~p~n", [GameId, EncryptionKey]),
    league_client:spectate_game(integer_to_list(GameId), binary_to_list(EncryptionKey)).
