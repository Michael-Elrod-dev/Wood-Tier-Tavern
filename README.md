# WoodTierTavern

WoodTierTavern is a Twitch-based betting platform, allowing viewers to bet on live Iron-ranked League of Legends matches. The project automatically finds live Iron games, spectates them for streaming, and manages viewer betting through Twitch chat integration and a MongoDB backend.

## Project Structure

The project consists of three main components:

```
WoodTierTavern/
├── WoodTierTavern/     # Game finding and spectating
├── WoodTierBookie/     # Twitch chat bot
└── WoodTierDatabase/   # MongoDB backend
```

### WoodTierTavern (Game Finder)

This component handles finding and spectating Iron-ranked games using Riot's API and the League Client API (LCU). Key features:

- Player Collection: Gathers Iron-ranked player information across all divisions
- Live Game Detection: Monitors collected players for active ranked games
- Spectator Mode: Automatically launches the League client in spectator mode for streaming
- API Integration: Interfaces with both Riot's public API and the League Client API

Key files:
- `playerCollector.js`: Gathers Iron-ranked player information
- `gameFetcher.js`: Monitors players for active games
- `leagueClient.js`: Handles League client interaction for spectating
- `utils.js`: Shared utility functions

### WoodTierBookie (Twitch Bot)

A Twitch chat bot that manages viewer interaction and betting. Features:

- Chat Commands: Processes viewer betting commands
- Token Management: Handles Twitch authentication
- Real-time Interaction: Responds to viewer commands and updates
- MongoDB Integration: Interfaces with database for user/bet management

Key files:
- `bot.js`: Main bot functionality and command handling
- `tokenManager.js`: Manages Twitch authentication tokens

### WoodTierDatabase (Backend)

MongoDB database system managing all betting and user data. Features:

- User Management: Tracks user balances, stats, and betting history
- Game Tracking: Records game outcomes and betting information
- Statistics: Maintains leaderboards and user achievements
- Transaction Management: Handles bet placement and payouts

Key files:
- `schemas.js`: MongoDB schema definitions
- `user-api.js`: User management endpoints
- `game-api.js`: Game and betting endpoints

## User Features

- Betting on Iron-ranked games through Twitch chat
- User progression system with ranks and achievements
- Stats tracking (win rate, biggest wins, streaks, etc.)
- Leaderboards and competitive elements
- Different user types (viewer, subscriber, VIP, moderator)

## Technical Implementation

- Uses Riot Games API for player and game data
- Integrates with League Client for spectating
- Twitch API integration for chat bot functionality
- MongoDB for persistent data storage
- Node.js backend architecture

## Current Status

This project is currently under development. The core components are being built and tested independently before integration.

## Note

This repository is primarily for code reference and documentation. Due to the complexity of dependencies and required API keys (Riot Games, Twitch), it's not intended for direct downloading and running.

## Requirements (if running locally)

- Node.js
- MongoDB
- League of Legends Client
- Riot Games API Key
- Twitch Developer Account
- Various npm packages