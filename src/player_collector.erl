-module(player_collector).
-export([start/0, collect_players/0]).

-define(API_KEY, get_api_key()).
-define(BASE_URL, "https://na1.api.riotgames.com").
-define(DIVISIONS, ["I", "II", "III", "IV"]).
-define(DELAY_BETWEEN_REQUESTS, 1200). % Add delay to avoid rate limits

get_api_key() ->
    case file:read_file("api_key.txt") of
        {ok, Binary} -> string:trim(binary_to_list(Binary));
        {error, _} -> throw({error, "API key not found in config/api_key.txt"})
    end.

start() ->
    case application:ensure_all_started([jsx, inets, ssl]) of
        {ok, _} ->
            collect_players();
        {error, Reason} ->
            io:format("Failed to start required applications: ~p~n", [Reason])
    end.

collect_players() ->
    lists:foreach(fun(Division) ->
        collect_division(Division)
    end, ?DIVISIONS).

collect_division(Division) ->
    Filename = "iron_" ++ string:lowercase(Division) ++ "_players.txt",
    io:format("Collecting Iron ~s players...~n", [Division]),
    
    Url = ?BASE_URL ++ "/lol/league/v4/entries/RANKED_SOLO_5x5/IRON/" ++ Division ++ "?page=100",
    Headers = [{"X-Riot-Token", ?API_KEY}],
    
    case httpc:request(get, {Url, Headers}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            Players = jsx:decode(list_to_binary(Body)),
            PUUIDs = get_puuids(Players),
            case write_to_file(Filename, PUUIDs) of
                ok ->
                    io:format("Saved ~p Iron ~s players to ~s~n", 
                             [length(PUUIDs), Division, Filename]);
                {error, Reason} ->
                    io:format("Error saving to ~s: ~p~n", [Filename, Reason])
            end;
        {ok, {{_, Code, _}, _, _}} ->
            io:format("API error getting Iron ~s players. Status code: ~p~n", 
                     [Division, Code]);
        Error ->
            io:format("Network error getting Iron ~s players: ~p~n", 
                     [Division, Error])
    end.

get_puuids(Players) ->
    lists:filtermap(fun(Player) ->
        SummonerId = maps:get(<<"summonerId">>, Player),
        case get_puuid(SummonerId) of
            {ok, PUUID} ->
                timer:sleep(?DELAY_BETWEEN_REQUESTS), % Add delay between requests
                {true, PUUID};
            {error, _} ->
                false
        end
    end, Players).

get_puuid(SummonerId) ->
    Url = ?BASE_URL ++ "/lol/summoner/v4/summoners/" ++ 
          uri_string:quote(binary_to_list(SummonerId)),
    Headers = [{"X-Riot-Token", ?API_KEY}],
    
    case httpc:request(get, {Url, Headers}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            SummonerInfo = jsx:decode(list_to_binary(Body)),
            {ok, binary_to_list(maps:get(<<"puuid">>, SummonerInfo))};
        {ok, {{_, 429, _}, _, _}} ->
            io:format("Rate limit hit, waiting 2 minutes...~n"),
            timer:sleep(120000),
            get_puuid(SummonerId);
        _ ->
            {error, failed}
    end.

write_to_file(Filename, PUUIDs) ->
    Content = string:join(PUUIDs, "\n"),
    file:write_file(Filename, Content).