// index.js
const { GameFetcher } = require('./gameFetcher');
const { PlayerCollector } = require('./playerCollector');

async function main() {
    const fetcher = new GameFetcher();
    const collector = new PlayerCollector();

    try {
        // First collect players if needed
        // await collector.start();
        
        // Then start looking for games
        await fetcher.start();
    } catch (error) {
        console.error('Error:', error.message);
    }
}

main();