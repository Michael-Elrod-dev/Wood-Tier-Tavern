const axios = require('axios');
const https = require('https');
const fs = require('fs');
const path = require('path');

class LeagueClient {
    async startSpectate(gameInfo) {
        console.log('\n=== Starting Spectate Process ===');
        console.log('Game Info received:', JSON.stringify(gameInfo, null, 2));
        
        try {
            console.log('Attempting to connect to League Client...');
            const credentials = await this.attemptClientConnect(3);
            console.log('Successfully connected to League Client');
            
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
            await new Promise(resolve => setTimeout(resolve, 2000));
            return this.attemptClientConnect(attempts - 1);
        }
    }

    findLockFile() {
        const possiblePaths = [
            'C:\\Riot Games\\League of Legends',
            'D:\\Riot Games\\League of Legends',
            'C:\\Program Files\\Riot Games\\League of Legends',
            'C:\\Program Files (x86)\\Riot Games\\League of Legends'
        ];

        for (const basePath of possiblePaths) {
            const lockfilePath = path.join(basePath, 'lockfile');
            console.log('Checking path:', lockfilePath);
            if (fs.existsSync(lockfilePath)) {
                console.log('Found lockfile at:', lockfilePath);
                return lockfilePath;
            }
        }

        throw new Error('Lockfile not found');
    }

    async connectToClient() {
        try {
            const lockfilePath = this.findLockFile();
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

    async makeRequest(credentials, method, endpoint, data = null) {
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
            data
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
        console.log('\n=== Launching Spectate Mode ===');
        
        try {
            const payload = {
                dropInSpectateGameId: gameInfo.gameId.toString(),
                gameQueueType: "string", // We might need to confirm the exact format expected here
                allowObserveMode: "string", // And here too
                puuid: gameInfo.puuid
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
            console.error('Error Details:', {
                message: error.message,
                status: error.response?.status,
                data: error.response?.data
            });
            throw error;
        }
    }
}

module.exports = { LeagueClient };