// spectateManager.js
const util = require('util');
const axios = require('axios');
const https = require('https');
const { exec } = require('child_process');
const execAsync = util.promisify(exec);
const { GameFetcher } = require('./gameFetcher');
const { LeagueClient } = require('./leagueClient');
const { delay, makeLcuRequest, makeLiveGameRequest } = require('./utils');

class SpectateManager {
    constructor() {
        this.isRunning = false;
        this.credentials = null;
        this.currentGameInfo = null;
        
        this.RETRY_ATTEMPS = 30;
        this.CHECK_DELAY = 5000;
        this.END_OF_GAME_DELAY = 10000;

        this.gameFetcher = new GameFetcher();
        this.leagueClient = new LeagueClient();
    }

    async start() {
        this.isRunning = true;
        console.log('\n=== Starting Continuous Spectate Manager ===');

        while (this.isRunning) {
            try {
                if (!this.credentials) {
                    this.credentials = await this.leagueClient.attemptClientConnect(this.RETRY_ATTEMPS);
                }

                await this.findAndSpectateGame();
                await this.waitForGameStart();
                await this.monitorActiveGame();
                await this.handlePostGame();

            } catch (error) {
                console.error('Spectate manager error:', error.message);
            }
        }
    }

    async findAndSpectateGame() {
        console.log('\nSearching for new game to spectate...');
        const gameFetcher = new GameFetcher();
        return await gameFetcher.start();
    }

    async waitForGameStart() {
        console.log('\nWaiting for game to start...');
        
        while (true) {
            const gameflowPhase = await this.getGameflowPhase();
            if (gameflowPhase === 'InProgress') {
                console.log('Game detected as InProgress, checking for GameStart event...');
                await this.waitForGameStartEvent();
                break;
            }
            await delay(this.CHECK_DELAY);
        }
    }

    async function waitForGameStartEvent() {
        while (true) {
            try {
                const gameData = await makeLiveGameRequest('GET', '/liveclientdata/allgamedata');
                const events = gameData.events.Events;
                
                if (events.some(event => event.EventName === "GameStart")) {
                    console.log('Game has started, updating spectator interface...');
                    await this.updateReplayInterface({
                        interfaceScoreboard: true,
                        interfaceTimeline: false,
                    });
                    return;
                }
            } catch (error) {
                await delay(this.CHECK_DELAY);
            }
        }
    }
    
    async function monitorActiveGame() {
        console.log('\nMonitoring active game for GameEnd event...');
        
        while (true) {
            try {
                const gameData = await makeLiveGameRequest('GET', '/liveclientdata/allgamedata');
                const events = gameData.events.Events;
                
                if (events.some(event => event.EventName === "GameEnd")) {
                    console.log('GameEnd event detected, proceeding to post-game handling...');
                    return;
                }
            } catch (error) {
                const gameflowPhase = await this.getGameflowPhase();
                if (!['InProgress', 'WaitingForStats'].includes(gameflowPhase)) {
                    return;
                }
            }
            await delay(this.END_OF_GAME_DELAY);
        }
    }
    
    async function updateReplayInterface(settings) {
        try {
            const response = await makeLiveGameRequest('POST', '/replay/render', settings);
            console.log('Successfully updated replay interface settings:', settings);
            return response;
        } catch (error) {
            console.error('Error updating replay interface:', error);
            throw error;
        }
    }

    async handlePostGame() {
        console.log('\nHandling post-game state...');
        await this.forceCloseGame();
        await this.killLeagueClient();

        this.credentials = null;
        
        console.log('Waiting for processes to clean up...');
        await delay(this.CHECK_DELAY);
        
        try {
            console.log('\nLaunching League client...');
            
            const shortcutPath = '"C:\\Riot Games\\Riot Client\\RiotClientServices.exe"';
            const args = '--launch-product=league_of_legends --launch-patchline=live';
            const command = `${shortcutPath} ${args}`;
    
            await execAsync(command);
        } catch (error) {
            console.error('Error launching client:', error);
            throw error;
        }
        await this.waitForClientState('None');
    }

    async forceCloseGame() {
        try {
            console.log('\nClosing League game process...');
            
            const { stdout } = await execAsync('wmic process where "name=\'League of Legends.exe\'" get processid');
            const pids = stdout.split('\n')
                .map(line => line.trim())
                .filter(line => /^\d+$/.test(line));

            if (pids.length > 0) {
                console.log(`Found League process: ${pids[0]}`);
                await execAsync(`wmic process ${pids[0]} delete`);
                console.log('Successfully terminated League game');
            } else {
                console.log('No League game process found');
            }
        } catch (error) {
            console.error('Error:', error);
            if (error.stdout) console.log('Standard output:', error.stdout);
            if (error.stderr) console.log('Error output:', error.stderr);
        }
    }

    async killLeagueClient() {
        try {
            console.log('\nClosing League client...');
            
            const { stdout } = await execAsync('wmic process where "name=\'LeagueClient.exe\'" get processid');
            const pids = stdout.split('\n')
                .map(line => line.trim())
                .filter(line => /^\d+$/.test(line));

            if (pids.length > 0) {
                console.log(`Found League client process: ${pids[0]}`);
                await execAsync(`wmic process ${pids[0]} delete`);
                console.log('Successfully terminated League client');
            } else {
                console.log('No League client process found');
            }
        } catch (error) {
            console.error('Error closing client:', error);
        }
    }

    async launchClient() {
        try {
            console.log('\nLaunching League client...');
            
            const shortcutPath = '"C:\\Riot Games\\Riot Client\\RiotClientServices.exe"';
            const args = '--launch-product=league_of_legends --launch-patchline=live';
            const command = `${shortcutPath} ${args}`;
    
            console.log('Executing command:', command);
            const result = await execAsync(command);
            console.log('League client launch initiated');
            
            return result;
        } catch (error) {
            console.error('Error launching client:', error);
            throw error;
        }
    }

    async waitForClientState(desiredState, maxAttempts = 60) {
        console.log(`\nWaiting for client to reach ${desiredState} state...`);
        let attempts = 0;
    
        while (attempts < maxAttempts) {
            try {
                const phase = await this.getGameflowPhase();
                console.log(`Current phase: ${phase}`);
                
                if (phase === desiredState) {
                    console.log(`Client reached ${desiredState} state`);
                    return true;
                }
            } catch (error) {
                attempts++;
            }
            await delay(this.CHECK_DELAY);
        }
        throw new Error(`Client failed to reach ${desiredState} state after ${maxAttempts} attempts`);
    }

    async getGameflowPhase() {
        try {
            const response = await makeLcuRequest(this.credentials, 'GET', '/lol-gameflow/v1/gameflow-phase');
            return response;
        } catch (error) {
            throw new Error(`Failed to get gameflow phase: ${error.message}`);
        }
    }

    stop() {
        this.isRunning = false;
        console.log('\nStopping Spectate Manager...');
    }
}

module.exports = { SpectateManager };