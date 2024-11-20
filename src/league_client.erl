-module(league_client).
-export([spectate_game/2]).

-define(GAME_PATH, "C:\\Riot Games\\League of Legends\\Game\\League of Legends.exe").
-define(SPECTATOR_SERVER, "spectator-consumer.akamaized.net:80").
-define(REGION, "NA1").

spectate_game(GameId, EncryptionKey) when is_list(GameId), is_list(EncryptionKey) ->
    EncArgs = string:join(["spectator", ?SPECTATOR_SERVER, EncryptionKey, GameId, ?REGION], " "),
    PsCommand = io_lib:format(
        "powershell.exe -NoProfile -ExecutionPolicy Bypass -Command \"Start-Process -FilePath '~s' -ArgumentList '~s' -Verb runAs\"",
        [?GAME_PATH, EncArgs]
    ),
    
    io:format("Generated Command: ~s~n", [PsCommand]),
    
    case os:cmd(PsCommand) of
        [] ->
            io:format("Successfully initiated spectator mode~n"),
            {ok, spectating};
        CmdError ->
            io:format("Failed to launch spectator mode: ~p~n", [CmdError]),
            {error, CmdError}
    end.
