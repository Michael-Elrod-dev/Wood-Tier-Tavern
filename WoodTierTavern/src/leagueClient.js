// leagueClient.js
const fs = require('fs');
const { findLockFile, delay, makeLcuRequest } = require('./utils');

class LeagueClient {
    constructor () {
        this.CHECK_DELAY = 5000;
        this.OBSERVE_MODE = "ALL";
        this.GAME_QUEUE_TYPE = "RANKED_SOLO_5x5";
    }

    async startSpectate(gameInfo) {        
        try {
            const credentials = await this.attemptClientConnect(3);            
            await this.launchSpectate(credentials, gameInfo);
            return credentials;
        } catch (error) {
            console.error('Spectate startup error:', error.message);
            throw error;
        }
    }

    async attemptClientConnect(attempts) {
        if (attempts === 0) throw new Error('League Client not running');

        try {
            return await this.connectToClient();
        } catch (error) {
            await delay(this.CHECK_DELAY);
            return this.attemptClientConnect(attempts - 1);
        }
    }

    async connectToClient() {
        try {
            const lockfilePath = findLockFile();
            const lockfileContent = fs.readFileSync(lockfilePath, 'utf8');
            const [, , port, password, protocol] = lockfileContent.split(':');

            return {
                port,
                auth: password
            };
        } catch (error) {
            console.error('Error reading lockfile:', error.message);
            throw error;
        }
    }

    async launchSpectate(credentials, gameInfo) {
        console.log('\n=== Launching Spectate ===');
        
        try {
            const encodedRiotId = encodeURIComponent(`${gameInfo.gameName}#${gameInfo.tagLine}`);            
            const summonerData = await makeLcuRequest(
                credentials,
                'GET',
                `/lol-summoner/v1/summoners?name=${encodedRiotId}`
            );            
            if (!summonerData || !summonerData.puuid) {
                throw new Error('Could not find internal PUUID for player');
            }
    
            const payload = {
                dropInSpectateGameId: gameInfo.gameId.toString(),
                gameQueueType: this.GAME_QUEUE_TYPE,
                allowObserveMode: this.OBSERVE_MODE,
                puuid: summonerData.puuid
            };
                
            const response = await makeLcuRequest(
                credentials,
                'POST',
                '/lol-spectator/v1/spectate/launch',
                payload
            );
                        
            return response;
        } catch (error) {
            console.error('\n=== Spectate Launch Error ===');
            console.error('Full error:', {
                status: error.response?.status,
                statusText: error.response?.statusText,
                data: error.response?.data,
                message: error.message
            });
            throw error;
        }
    }
}

module.exports = { LeagueClient };