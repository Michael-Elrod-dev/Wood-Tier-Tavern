// gameFetcher.js
const axios = require('axios');
const fs = require('fs/promises');
const { LeagueClient } = require('./leagueClient');
const { getApiKey, delay, shuffleList, getRiotHeaders, updateProgress } = require('./utils');

class GameFetcher {
    constructor() {
        this.API_KEY = null;
        this.BASE_URL = 'https://na1.api.riotgames.com';
        this.AMERICAS_URL = 'https://americas.api.riotgames.com';
        this.DELAY_BETWEEN_CHECKS = 1300;
        this.IRON_FILES = [
            'files/iron_i_players.json',
            'files/iron_ii_players.json',
            'files/iron_iii_players.json',
            'files/iron_iv_players.json'
        ];
    }

    async readPlayerFile(filename) {
        try {
            const content = await fs.readFile(filename, 'utf8');
            const jsonData = JSON.parse(content);

            return jsonData.players
                .filter(player =>
                    player.summonerId &&
                    player.puuid &&
                    player.gameName &&
                    player.tagLine
                );
        } catch (error) {
            console.error(`Error reading ${filename}:`, error);
            return [];
        }
    }

    async checkActiveGame(player) {
        const url = `${this.BASE_URL}/lol/spectator/v5/active-games/by-summoner/${encodeURIComponent(player.puuid)}`;
        const headers = getRiotHeaders(this.API_KEY);
    
        await delay(this.DELAY_BETWEEN_CHECKS);
    
        try {
            const response = await axios.get(url, { headers });
            const rankedQueueIds = [420];
    
            if (rankedQueueIds.includes(response.data.gameQueueConfigId)) {
                const participant = response.data.participants.find(p => p.puuid === player.puuid);
                if (!participant) {
                    return { found: false };
                }
    
                // Check if game has been running for less than 3 minutes
                if (response.data.gameLength <= 180) {
                    return {
                        found: true,
                        gameId: response.data.gameId,
                        encryptionKey: response.data.observers.encryptionKey,
                        gameQueueConfigId: response.data.gameQueueConfigId,
                        summonerId: player.summonerId,
                        puuid: player.puuid,
                        gameName: player.gameName,
                        tagLine: player.tagLine,
                        gameLength: response.data.gameLength
                    };
                } else {
                    console.log(`\nGame found but has been running for ${Math.floor(response.data.gameLength / 60)}:${(response.data.gameLength % 60).toString().padStart(2, '0')} minutes`);
                    return { found: false };
                }
            }
            return { found: false };
        } catch (error) {
            if (error.response?.status === 404) {
                return { found: false };
            }
            console.error('Active game check error details:', {
                status: error.response?.status,
                data: error.response?.data,
                headers: error.response?.headers
            });
            throw new Error(`API error: ${error.response?.status || error.message}`);
        }
    }

    async checkPlayers(players, count) {
        if (players.length === 0) {
            console.log('\nFinished checking all players. No active games found');
            return;
        }

        const player = players[0];
        updateProgress(count + 1, players.length, 'Checking player');

        try {
            const gameInfo = await this.checkActiveGame(player);

            if (gameInfo.found) {
                const minutes = Math.floor(gameInfo.gameLength / 60);
                const seconds = gameInfo.gameLength % 60;
                
                console.log(`\nFound active game! Game running for ${minutes}:${seconds.toString().padStart(2, '0')}. Attempting to spectate...`);
                
                const leagueClient = new LeagueClient();
                try {
                    await leagueClient.startSpectate(gameInfo);
                    console.log('Spectate process completed - ending search');
                    return;
                } catch (error) {
                    console.error('Failed to spectate game:', error.message);
                    console.log('Ending search due to found game, even though spectate failed');
                    return;
                }
            } else {
                await this.checkPlayers(players.slice(1), count + 1);
            }
        } catch (error) {
            console.error('\nError checking player:', error);
            await this.checkPlayers(players.slice(1), count + 1);
        }
    }

    async start() {
        this.API_KEY = await getApiKey();

        const allPlayers = [];
        for (const file of this.IRON_FILES) {
            const players = await this.readPlayerFile(file);
            allPlayers.push(...players);
        }

        if (allPlayers.length === 0) {
            console.log('No players found in files');
            return;
        }

        const shuffledPlayers = shuffleList(allPlayers);
        await this.checkPlayers(shuffledPlayers, 0);
    }
}

module.exports = { GameFetcher };