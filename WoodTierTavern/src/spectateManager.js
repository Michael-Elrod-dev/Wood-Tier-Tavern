// spectateManager.js
const axios = require('axios');
const https = require('https');
const { GameFetcher } = require('./gameFetcher');
const { LeagueClient } = require('./leagueClient');
const { delay, makeLcuRequest } = require('./utils');

class SpectateManager {
    constructor() {
        this.isRunning = false;
        this.credentials = null;
        this.currentGameInfo = null;
        this.ERROR_DELAY = 5000;
        this.NEW_SEARCH_DELAY = 200000;
        this.DELAY_BETWEEN_CHECKS = 30000;
        this.gameFetcher = new GameFetcher();
        this.leagueClient = new LeagueClient();
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

            } catch (error) {
                console.error('Spectate manager error:', error.message);
                await delay(this.ERROR_DELAY);
            }
        }
    }

    async findAndSpectateGame() {
        console.log('\nSearching for new game to spectate...');

        const gameFetcher = new GameFetcher();
        const gameFound = await gameFetcher.start();

        if (!gameFound) {
            return false;
        }

        console.log(`\nWaiting ${this.NEW_SEARCH_DELAY / 1000} seconds for spectate to fully load...`);
        await delay(this.NEW_SEARCH_DELAY);

        // Update replay interface settings after load
        await this.updateReplayInterface({
            interfaceScoreboard: true,
            interfaceTimeline: false,
        });

        return true;
    }

    async updateReplayInterface(settings) {
        try {
            const response = await axios({
                method: 'POST',
                url: 'https://127.0.0.1:2999/replay/render',
                data: settings,
                httpsAgent: new https.Agent({
                    rejectUnauthorized: false
                })
            });
            console.log('Successfully updated replay interface settings:', settings);
            return response.data;
        } catch (error) {
            console.error('Error updating replay interface:', error);
            throw error;
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

                process.stdout.clearLine(0);
                process.stdout.cursorTo(0);
                process.stdout.write(`Check #${checkCount} - Current gameflow phase: ${gameflowPhase}`);

                if (['None', 'Lobby'].includes(gameflowPhase)) {
                    process.stdout.write('\nGame detected as ended\n');
                    await this.forceCloseGame();
                    gameEnded = true;
                    break;
                }

                if (['InProgress', 'WaitingForStats', 'PreEndOfGame', 'EndOfGame'].includes(gameflowPhase)) {
                    process.stdout.clearLine(0);
                    process.stdout.cursorTo(0);
                    process.stdout.write(`Check #${checkCount} - Game still active (${gameflowPhase}) - checking again in 30 seconds`);
                    await delay(this.DELAY_BETWEEN_CHECKS);
                } else {
                    process.stdout.clearLine(0);
                    process.stdout.cursorTo(0);
                    process.stdout.write(`Check #${checkCount} - Unknown state: ${gameflowPhase} - checking again in 30 seconds`);
                    await delay(this.DELAY_BETWEEN_CHECKS);
                }
            } catch (error) {
                process.stdout.write(`\nError monitoring game state: ${error}\n`);
                await delay(this.DELAY_BETWEEN_CHECKS);
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