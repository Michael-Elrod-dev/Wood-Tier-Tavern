const axios = require('axios');
const fs = require('fs/promises');
const { LeagueClient } = require('./leagueClient');

class GameFetcher {
   constructor() {
       this.API_KEY = null;
       this.BASE_URL = 'https://na1.api.riotgames.com';
       this.AMERICAS_URL = 'https://americas.api.riotgames.com';
       this.DELAY_BETWEEN_CHECKS = 1500;
       this.IRON_FILES = [
           'files/iron_i_players.txt',
           'files/iron_ii_players.txt',
           'files/iron_iii_players.txt',
           'files/iron_iv_players.txt'
       ];
   }

   async getApiKey() {
       try {
           this.API_KEY = (await fs.readFile('api_key.txt', 'utf8')).trim();
       } catch (error) {
           throw new Error('API key not found in config/api_key.txt');
       }
   }

   async readPlayerFile(filename) {
        try {
            const content = await fs.readFile(filename, 'utf8');
            return content.split('\n')
                .slice(1) // Skip the first line (header)
                .map(line => {
                    const [summonerId, puuid] = line.trim().split(',');
                    return { summonerId, puuid };
                })
                .filter(player => player.summonerId && player.puuid);
        } catch (error) {
            console.error(`Error reading ${filename}:`, error);
            return [];
        }
    }

   shuffleList(list) {
       return list
           .map(value => ({ value, sort: Math.random() }))
           .sort((a, b) => a.sort - b.sort)
           .map(({ value }) => value);
   }

   delay(ms) {
       return new Promise(resolve => setTimeout(resolve, ms));
   }

   async checkActiveGame(puuid) {
    const url = `${this.BASE_URL}/lol/spectator/v5/active-games/by-summoner/${encodeURIComponent(puuid)}`;
    const headers = { 'X-Riot-Token': this.API_KEY };

    await this.delay(this.DELAY_BETWEEN_CHECKS);

    try {
        const response = await axios.get(url, { headers });
        const rankedQueueIds = [420];
        
        if (rankedQueueIds.includes(response.data.gameQueueConfigId)) {
            return {
                found: true,
                gameId: response.data.gameId,
                encryptionKey: response.data.observers.encryptionKey,
                gameQueueConfigId: response.data.gameQueueConfigId,
                puuid
            };
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

   async start() {
       await this.getApiKey();
       
       const allPlayers = [];
       for (const file of this.IRON_FILES) {
           const players = await this.readPlayerFile(file);
           allPlayers.push(...players);
       }

       if (allPlayers.length === 0) {
           console.log('No players found in files');
           return;
       }

       console.log(`Found ${allPlayers.length} players to check`);
       const shuffledPlayers = this.shuffleList(allPlayers);
       await this.checkPlayers(shuffledPlayers, 0);
   }

   async checkPlayers(players, count) {
        if (players.length === 0) {
            console.log('\nFinished checking all players. No active games found');
            return;
        }

        const player = players[0];
        process.stdout.write(`\rChecking player ${count + 1}/${players.length}...`);

        try {
            const gameResult = await this.checkActiveGame(player.puuid);

            if (gameResult.found) {
                console.log('\nFound active game! Attempting to spectate...');
                const leagueClient = new LeagueClient();
                try {
                    await leagueClient.startSpectate(gameResult);
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
}

module.exports = { GameFetcher };