const axios = require('axios');
const fs = require('fs/promises');
const path = require('path');

class PlayerCollector {
    constructor() {
        this.API_KEY = null;
        this.BASE_URL = 'https://na1.api.riotgames.com';
        this.DIVISIONS = ['I', 'II', 'III', 'IV'];
        this.DELAY_BETWEEN_REQUESTS = 1500;
    }

    async getApiKey() {
        try {
            this.API_KEY = (await fs.readFile('api_key.txt', 'utf8')).trim();
        } catch (error) {
            throw new Error('API key not found in config/api_key.txt');
        }
    }

    delay(ms) {
        return new Promise(resolve => setTimeout(resolve, ms));
    }

    async start() {
        try {
            await this.getApiKey();
            try {
                await fs.mkdir('files', { recursive: true });
            } catch (err) {
                if (err.code !== 'EEXIST') throw err;
            }
            await this.collectPlayers();
        } catch (error) {
            console.error('Failed to start:', error.message);
        }
    }

    async collectPlayers() {
        for (const division of this.DIVISIONS) {
            await this.collectDivision(division);
        }
    }

    async collectDivision(division) {
        const filename = path.join('files', `iron_${division.toLowerCase()}_players.txt`);
        console.log(`Collecting Iron ${division} players...`);

        const url = `${this.BASE_URL}/lol/league/v4/entries/RANKED_SOLO_5x5/IRON/${division}?page=1`;
        const headers = { 
            'X-Riot-Token': this.API_KEY,
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36',
            'Accept-Language': 'en-US,en;q=0.9',
            'Accept-Charset': 'application/x-www-form-urlencoded; charset=UTF-8'
        };

        try {
            const response = await axios.get(url, { headers });
            console.log(`Got ${response.data.length} players from Iron ${division}`);
            
            // Get both summoner ID and PUUID for each player
            const playerData = [];
            for (const entry of response.data) {
                try {
                    await this.delay(this.DELAY_BETWEEN_REQUESTS); // Respect rate limits
                    const summonerUrl = `${this.BASE_URL}/lol/summoner/v4/summoners/${entry.summonerId}`;
                    const summonerResponse = await axios.get(summonerUrl, { headers });
                    playerData.push({
                        summonerId: entry.summonerId,
                        puuid: summonerResponse.data.puuid
                    });
                    console.log(`Retrieved data for summoner: ${entry.summonerId}`);
                } catch (error) {
                    console.error(`Failed to get PUUID for summoner ${entry.summonerId}:`, error.message);
                }
            }
            
            // Format as CSV without space after comma for easier parsing
            const formattedData = playerData.map(player => `${player.summonerId},${player.puuid}`);
            
            await this.writeToFile(filename, formattedData);
            console.log(`Saved ${formattedData.length} players to ${filename}`);
        } catch (error) {
            if (error.response) {
                console.error(`API error getting Iron ${division} players:`, {
                    status: error.response.status,
                    data: error.response.data
                });
            } else {
                console.error(`Network error getting Iron ${division} players:`, error.message);
            }
        }
    }

    async writeToFile(filename, data) {
        try {
            if (!data || data.length === 0) {
                console.error('No data to write to file');
                return false;
            }
            await fs.writeFile(filename, data.join('\n'));
            return true;
        } catch (error) {
            console.error(`Error saving to ${filename}:`, error.message);
            return false;
        }
    }
}

module.exports = { PlayerCollector };