# Wood Tier Tavern

## Overview

**Wood Tier Tavern** is an Erlang application designed to interact with Riot Games' League of Legends API to find and connect to active games involving low-ELO players (Iron rank). It fetches and processes data using API requests and provides functionality to interact with spectator services.

---

## Features

- Fetches ranked player data from the Riot API.
- Finds active games by querying summoner and match data.
- Includes a customizable delay and limit for checking players.
- Outputs relevant game details such as Game ID and encryption key.

---

## Usage

### Starting the Application

Run the application using:

```erlang
game_fetcher:start().
```
This will start all necessary dependencies (jsx, inets, ssl) and call the test_api/0 function to fetch data from the Riot API.

### Configuration
Update the following macros in the code as needed:

- API Key: Replace "...“ in ?API_KEY with your valid Riot Games API Key.
- Base URL: Adjust ?BASE_URL if using a region-specific endpoint.
- Maximum Players to Check: Modify ?MAX_PLAYERS_TO_CHECK to control the number of players to analyze.
- Delay Between Checks: Adjust ?DELAY_BETWEEN_CHECKS for the time (in milliseconds) between API calls.

### Functions
`start/0`
- Initializes the application by starting dependencies and calling test_api/0.

`test_api/0`
- Fetches Iron IV players from the Riot API and begins the process of finding active games.

`check_players/2`
- Iterates through a list of players and checks for active games using their summonerId.

`check_player_game/1`
- Retrieves puuid and checks if the summoner is currently in an active game.

`connect_to_game/1`
- Outputs game details (GameId, EncryptionKey) for connecting to the spectator service. (To be implemented.)

## Requirements
Dependencies:
- jsx for JSON parsing.
- inets and ssl for HTTP and HTTPS requests.

## License
This project is licensed under the MIT License.
