// leagueClient.js
const axios = require('axios');
const https = require('https');
const fs = require('fs');
const path = require('path');
const { findLockFile, delay } = require('./utils');

class LeagueClient {
    async startSpectate(gameInfo) {
        console.log('Game Info retrieved:', JSON.stringify(gameInfo, null, 2));
        
        try {
            const credentials = await this.attemptClientConnect(3);            
            return this.launchSpectate(credentials, gameInfo);
        } catch (error) {
            console.error('Spectate startup error:', error.message);
            return { error: error.message };
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
            console.log('Lockfile content:', lockfileContent);

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

    async makeRequest(credentials, method, endpoint, payload = null) {
        const url = `https://127.0.0.1:${credentials.port}${endpoint}`;
        const auth = Buffer.from(`riot:${credentials.auth}`).toString('base64');
        
        console.log(`Making request to: ${url}`);
        console.log('Using auth token:', auth);
        
        const config = {
            method,
            url,
            headers: {
                'Authorization': `Basic ${auth}`,
                'Content-Type': 'application/json'
            },
            httpsAgent: new https.Agent({
                rejectUnauthorized: false
            }),
            data: payload
        };
    
        try {
            const response = await axios(config);
            return response.data;
        } catch (error) {
            console.log('Full error:', {
                status: error.response?.status,
                statusText: error.response?.statusText,
                data: error.response?.data,
                message: error.message
            });
            throw new Error(`LCU request failed: ${error.response?.status} ${error.response?.statusText}`);
        }
    }

    async launchSpectate(credentials, gameInfo) {
        console.log('\n=== Launching Spectate ===');
        const riotId = `${gameInfo.gameName}#${gameInfo.tagLine}`;
        
        try {
            // URL encode the entire Riot ID (name#tag format)
            const encodedRiotId = encodeURIComponent(`${gameInfo.gameName}#${gameInfo.tagLine}`);
            console.log('Getting internal summoner info for:', riotId);
            
            const summonerData = await this.makeRequest(
                credentials,
                'GET',
                `/lol-summoner/v1/summoners?name=${encodedRiotId}`
            );
            
            console.log('Internal summoner data:', summonerData);
            
            if (!summonerData || !summonerData.puuid) {
                throw new Error('Could not find internal PUUID for player');
            }
    
            const payload = {
                dropInSpectateGameId: gameInfo.gameId.toString(),
                gameQueueType: "RANKED_SOLO_5x5",
                allowObserveMode: "ALL",
                puuid: summonerData.puuid
            };
    
            console.log('\nPrepared spectate payload:', JSON.stringify(payload, null, 2));
            
            const response = await this.makeRequest(
                credentials,
                'POST',
                '/lol-spectator/v1/spectate/launch',
                payload
            );
    
            console.log('\nSpectate launch response:', JSON.stringify(response, null, 2));
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