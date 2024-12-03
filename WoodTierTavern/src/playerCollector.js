const path = require('path');
const fs = require('fs/promises');
const { getApiKey, delay, writeJsonToFile, updateProgress, makeRiotRequest } = require('./utils');

class PlayerCollector {
    constructor() {
        this.API_KEY = null;
        this.BASE_URL = 'https://na1.api.riotgames.com';
        this.RIOT_URL = 'https://americas.api.riotgames.com';
        this.DIVISIONS = ['I', 'II', 'III', 'IV'];
        this.DELAY_BETWEEN_REQUESTS = 1500;
    }

    async start() {
        try {
            this.API_KEY = await getApiKey();
            await fs.mkdir('../files', { recursive: true });
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
        const filename = path.join('../files', `iron_${division.toLowerCase()}_players.json`);
        console.log(`\nStarting Iron ${division} collection...`);

        try {
            const summonerIds = await this.getSummonerIds(division);
            await delay(this.DELAY_BETWEEN_REQUESTS);
            console.log(`Found ${summonerIds.length} players in Iron ${division}`);

            const playerData = [];
            let processed = 0;

            for (const summonerId of summonerIds) {
                try {
                    const puuid = await this.getPUUID(summonerId);
                    await delay(this.DELAY_BETWEEN_REQUESTS);
                    
                    const accountInfo = await this.getGameNameAndTagLine(puuid);
                    await delay(this.DELAY_BETWEEN_REQUESTS);
                    
                    playerData.push({
                        summonerId,
                        puuid,
                        gameName: accountInfo.gameName,
                        tagLine: accountInfo.tagLine
                    });
                    
                    processed++;
                    updateProgress(processed, summonerIds.length, `Processing Iron ${division}`);
                } catch (error) {
                    console.error(`\nError with summoner ${summonerId}:`, error.message);
                }
            }

            console.log('\nSaving data...');
            await writeJsonToFile(filename, playerData);
            console.log(`Completed Iron ${division}: Saved ${playerData.length} players\n`);
        } catch (error) {
            console.error(`\nError processing Iron ${division}:`, error.message);
        }
    }

    async getSummonerIds(division) {
        const allSummonerIds = [];
        let page = 1;
        const maxPages = 25;
    
        while (page <= maxPages) {
            const url = `${this.BASE_URL}/lol/league/v4/entries/RANKED_SOLO_5x5/IRON/${division}?page=${page}`;
            const data = await makeRiotRequest(this.API_KEY, url);
            
            if (data.length === 0) break;
            
            allSummonerIds.push(...data.map(entry => entry.summonerId));
            page++;
        }
    
        return allSummonerIds;
    }

    async getPUUID(summonerId) {
        const url = `${this.BASE_URL}/lol/summoner/v4/summoners/${summonerId}`;
        const data = await makeRiotRequest(this.API_KEY, url);
        return data.puuid;
    }
    
    async getGameNameAndTagLine(puuid) {
        const url = `${this.RIOT_URL}/riot/account/v1/accounts/by-puuid/${puuid}`;
        const data = await makeRiotRequest(this.API_KEY, url);
        return {
            gameName: data.gameName,
            tagLine: data.tagLine
        };
    }
}

module.exports = { PlayerCollector };