// index.js
const { GameFetcher } = require('./gameFetcher');
const { PlayerCollector } = require('./playerCollector');

async function main() {
    const fetcher = new GameFetcher();
    const collector = new PlayerCollector();

    try {
        // await collector.start();
        await fetcher.start();
    } catch (error) {
        console.error('Error:', error.message);
    }
}

main();