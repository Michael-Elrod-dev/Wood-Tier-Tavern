const axios = require('axios');
const https = require('https');
const { execSync } = require('child_process');

class LeagueClient {
    async startSpectate(name, tag, puuid) {
        try {
            const credentials = await this.attemptClientConnect(3);
            return this.launchSpectate(credentials, puuid);
        } catch (error) {
            console.error('Error:', error.message);
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

    async connectToClient() {
        let cmdOutput;
        try {
            // Add logging to see what's happening
            console.log('Attempting to connect to League Client...');
            
            // Try multiple methods to get the client info
            try {
                console.log('Attempting Get-CimInstance...');
                cmdOutput = execSync(
                    'powershell.exe -Command "Get-CimInstance Win32_Process -Filter \\"name = \'LeagueClientUx.exe\'\\" | Select-Object CommandLine | Format-List"',
                    { encoding: 'utf8' }
                );
            } catch (firstError) {
                console.log('Get-CimInstance failed, trying Get-Process...');
                try {
                    cmdOutput = execSync(
                        'powershell.exe -Command "Get-Process LeagueClientUx | Select-Object CommandLine | Format-List"',
                        { encoding: 'utf8' }
                    );
                } catch (secondError) {
                    console.log('Get-Process failed, trying WMIC...');
                    cmdOutput = execSync(
                        'wmic process where name="LeagueClientUx.exe" get commandline /format:list',
                        { encoding: 'utf8' }
                    );
                }
            }
    
            console.log('Raw command output:', cmdOutput);
    
            const credentials = this.parseClientCredentials(cmdOutput);
            if (!credentials) {
                console.log('Failed to parse credentials from output');
                throw new Error('Failed to extract client credentials');
            }
            
            console.log('Successfully found credentials - Port:', credentials.port);
            return credentials;
        } catch (error) {
            console.log('All connection attempts failed:', error.message);
            throw new Error('League Client not running');
        }
    }
    
    parseClientCredentials(cmdOutput) {
        console.log('Parsing credentials from output...');
        
        // Try to match the port and auth token using more flexible regex
        const portMatch = cmdOutput.match(/--app-port[=\s](\d+)/i);
        const authMatch = cmdOutput.match(/--remoting-auth-token[=\s]([\w-]+)/i);
        
        if (!portMatch || !authMatch) {
            console.log('Port match:', !!portMatch);
            console.log('Auth match:', !!authMatch);
            return null;
        }
        
        return {
            port: portMatch[1],
            auth: authMatch[1]
        };
    }

    async makeRequest(credentials, method, endpoint, data = null) {
        const url = `https://127.0.0.1:${credentials.port}${endpoint}`;
        const auth = Buffer.from(`riot:${credentials.auth}`).toString('base64');
        
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
            throw new Error(`LCU request failed: ${error.response?.data || error.message}`);
        }
    }

    async launchSpectate(credentials, puuid) {
        const payload = {
            allowObserveMode: "ALL",
            dropInSpectateGameId: "",
            gameQueueType: "",
            puuid: puuid
        };

        return this.makeRequest(
            credentials,
            'POST',
            '/lol-spectator/v1/spectate/launch',
            payload
        );
    }
}

module.exports = { LeagueClient };