# Wood Tier Tavern

A Node.js application that automatically finds and spectates ranked games of Iron-ranked players in League of Legends.

## Features

- Collects player data from Iron divisions I-IV
- Monitors Iron players for active ranked games
- Automatically launches the spectator mode when a suitable game is found
- Integrates with the League Client for seamless spectating

## Prerequisites

- Node.js installed
- League of Legends client installed
- Valid Riot Games API key

## Setup

1. Create a file named `api_key.txt` in the root directory and paste your Riot Games API key
2. Install dependencies:
   ```bash
   npm install
   ```

## Usage

The application has two main functionalities:

### 1. Collecting Player Data

To collect Iron player data:

```bash
node index.js
```

This will:
- Create a `files` directory
- Collect player data from all Iron divisions
- Save player information to separate files for each division

### 2. Finding and Spectating Games

To find and spectate games:
- Ensure League of Legends client is running
- Uncomment the `fetcher.start()` line in `index.js`
- Run:
  ```bash
  node index.js
  ```

The application will:
- Load collected player data
- Check players for active ranked games
- Automatically launch spectator mode when a suitable game is found

## Files Structure

- `index.js` - Main application entry point
- `playerCollector.js` - Handles collection of Iron player data
- `gameFetcher.js` - Finds active games among collected players
- `leagueClient.js` - Manages interaction with the League client
- `files/` - Directory containing collected player data

## Technical Details

- Uses Riot Games API for player data collection and game monitoring
- Interfaces with League Client API for spectating
- Implements rate limiting to comply with API restrictions
- Handles automatic authentication with the League client

## Error Handling

- Gracefully handles API errors and rate limits
- Retries League client connection if needed
- Provides detailed error logging for troubleshooting

## Notes

- API requests are rate-limited to comply with Riot Games API restrictions
- The application requires an active League client session for spectating
- Player data collection and game spectating can be run separately

## Limitations

- Only works with NA region (can be modified for other regions)
- Requires valid API key with appropriate rate limits
- Only searches for ranked solo queue games