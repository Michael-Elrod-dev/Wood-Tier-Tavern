// spectateManager.js
const { GameFetcher } = require('./gameFetcher');
const { LeagueClient } = require('./leagueClient');
const { delay, makeLcuRequest } = require('./utils');

class SpectateManager {
    constructor() {
        this.gameFetcher = new GameFetcher();
        this.leagueClient = new LeagueClient();
        this.isRunning = false;
        this.currentGameInfo = null;
        this.credentials = null;
    }

    async start() {
        this.isRunning = true;
        console.log('\n=== Starting Continuous Spectate Manager ===');
        
        while (this.isRunning) {
            try {
                if (!this.credentials) {
                    this.credentials = await this.leagueClient.attemptClientConnect(3);
                }

                await this.findAndSpectateGame();
                await this.monitorGameState();

                console.log('\nWaiting 10 seconds before starting next search...');
                await delay(10000);
            } catch (error) {
                console.error('Spectate manager error:', error.message);
                await delay(5000);
            }
        }
    }

    async findAndSpectateGame() {
        console.log('\nSearching for new game to spectate...');
        
        const gameFetcher = new GameFetcher();
        await gameFetcher.start();
        
        console.log('\nWaiting 10 minutes for spectate to fully load...');
        await delay(600000);
     
        await this.checkRenderProperties();
     }
     
     async checkRenderProperties() {
        try {
            const response = await fetch('https://127.0.0.1:2999/replay/render', {
                method: 'GET',
                rejectUnauthorized: false
            });
            const data = await response.json();
            console.log('Available render properties:', data);
            return data;
        } catch (error) {
            console.error('Error fetching render properties:', error);
            return null;
        }
     }

    async monitorGameState() {
        let gameEnded = false;
        let checkCount = 0;
        
        process.stdout.write('\nStarting game state monitoring...\n');
        
        while (!gameEnded && this.isRunning) {
            try {
                checkCount++;
                const gameflowPhase = await this.getGameflowPhase();
                
                const activeGame = await makeLcuRequest(
                    this.credentials,
                    'GET',
                    '/lol-spectator/v1/spectate/launch'
                ).catch(() => null);
    
                process.stdout.clearLine(0);
                process.stdout.cursorTo(0);
                process.stdout.write(`Check #${checkCount} - Current gameflow phase: ${gameflowPhase}`);
    
                if (!activeGame || gameflowPhase === 'None' || gameflowPhase === 'Lobby' || gameflowPhase === 'ReadyCheck') {
                    process.stdout.write('\nGame detected as ended\n');
                    await this.forceCloseGame();
                    gameEnded = true;
                    break;
                }
    
                if (['InProgress', 'WaitingForStats', 'PreEndOfGame', 'EndOfGame'].includes(gameflowPhase)) {
                    process.stdout.clearLine(0);
                    process.stdout.cursorTo(0);
                    process.stdout.write(`Check #${checkCount} - Game still active (${gameflowPhase}) - checking again in 30 seconds`);
                    await delay(30000);
                } else {
                    process.stdout.clearLine(0);
                    process.stdout.cursorTo(0);
                    process.stdout.write(`Check #${checkCount} - Unknown state: ${gameflowPhase} - checking again in 30 seconds`);
                    await delay(30000);
                }
            } catch (error) {
                process.stdout.write(`\nError monitoring game state: ${error}\n`);
                await delay(30000);
            }
        }
    }
    
    async forceCloseGame() {
        try {
            await makeLcuRequest(
                this.credentials,
                'POST',
                '/lol-gameflow/v1/session/game-end'
            );
    
            await makeLcuRequest(
                this.credentials,
                'POST',
                '/process-control/v1/process/quit'
            );
    
            console.log('Successfully closed game client');
        } catch (error) {
            console.error('Error closing game:', error);
        }
    }

    async getGameflowPhase() {
        try {
            const response = await makeLcuRequest(
                this.credentials,
                'GET',
                '/lol-gameflow/v1/gameflow-phase'
            );
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