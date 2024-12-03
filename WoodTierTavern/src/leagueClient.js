// leagueClient.js
const fs = require('fs');
const { findLockFile, delay, makeLcuRequest } = require('./utils');

class LeagueClient {
    async startSpectate(gameInfo) {
        // console.log('Game Info retrieved:', JSON.stringify(gameInfo, null, 2));
        
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
            await delay(2000);
            return this.attemptClientConnect(attempts - 1);
        }
    }

    async connectToClient() {
        try {
            const lockfilePath = findLockFile();
            const lockfileContent = fs.readFileSync(lockfilePath, 'utf8');
            // console.log('Lockfile content:', lockfileContent);

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
            // console.log('Getting internal summoner info for:', riotId);
            
            const summonerData = await makeLcuRequest(
                credentials,
                'GET',
                `/lol-summoner/v1/summoners?name=${encodedRiotId}`
            );
            
            // console.log('Internal summoner data:', summonerData);
            
            if (!summonerData || !summonerData.puuid) {
                throw new Error('Could not find internal PUUID for player');
            }
    
            const payload = {
                dropInSpectateGameId: gameInfo.gameId.toString(),
                gameQueueType: "RANKED_SOLO_5x5",
                allowObserveMode: "ALL",
                puuid: summonerData.puuid
            };
    
            // console.log('\nPrepared spectate payload:', JSON.stringify(payload, null, 2));
            
            const response = await makeLcuRequest(
                credentials,
                'POST',
                '/lol-spectator/v1/spectate/launch',
                payload
            );
    
            // console.log('\nSpectate launch response:', JSON.stringify(response, null, 2));
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