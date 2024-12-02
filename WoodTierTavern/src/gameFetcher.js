// gameFetcher.js
const { LeagueClient } = require('./leagueClient');
const { getApiKey, delay, readPlayerFile, shuffleList, updateProgress, makeRiotRequest } = require('./utils');

class GameFetcher {
    constructor() {
        this.API_KEY = null;
        this.BASE_URL = 'https://na1.api.riotgames.com';
        this.AMERICAS_URL = 'https://americas.api.riotgames.com';
        this.DELAY_BETWEEN_CHECKS = 1300;
        this.IRON_FILES = [
            '../files/iron_i_players.json',
            '../files/iron_ii_players.json',
            '../files/iron_iii_players.json',
            '../files/iron_iv_players.json'
        ];
    }

    async checkActiveGame(player) {
        const url = `${this.BASE_URL}/lol/spectator/v5/active-games/by-summoner/${encodeURIComponent(player.puuid)}`;
        await delay(this.DELAY_BETWEEN_CHECKS);
    
        try {
            const response = await makeRiotRequest(this.API_KEY, url);
            const rankedQueueIds = [420];
    
            if (rankedQueueIds.includes(response.gameQueueConfigId)) {
                const participant = response.participants.find(p => p.puuid === player.puuid);
                if (!participant) {
                    return { found: false };
                }
    
                if (response.gameLength <= 180) {
                    return {
                        found: true,
                        gameId: response.gameId,
                        encryptionKey: response.observers.encryptionKey,
                        gameQueueConfigId: response.gameQueueConfigId,
                        summonerId: player.summonerId,
                        puuid: player.puuid,
                        gameName: player.gameName,
                        tagLine: player.tagLine,
                        gameLength: response.gameLength
                    };
                } else {
                    console.log(`\nGame found but has been running for ${Math.floor(response.gameLength / 60)}:${(response.gameLength % 60).toString().padStart(2, '0')} minutes`);
                    return { found: false };
                }
            }
            return { found: false };
        } catch (error) {
            if (error.response?.status === 404) {
                return { found: false };
            }
            throw error;
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
            const players = await readPlayerFile(file);
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