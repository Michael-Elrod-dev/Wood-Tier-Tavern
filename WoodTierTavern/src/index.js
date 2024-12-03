// index.js
const { PlayerCollector } = require('./playerCollector');
const { SpectateManager } = require('./spectateManager');

const MODES = {
    COLLECT: 'collect',
    SPECTATE: 'spectate'
};

async function main() {
    const mode = process.argv[2]?.toLowerCase() || MODES.SPECTATE;

    try {
        if (mode === MODES.COLLECT) {
            const collector = new PlayerCollector();
            await collector.start();
        } else if (mode === MODES.SPECTATE) {
            const manager = new SpectateManager();
            await manager.start();
        } else {
            console.error(`Invalid mode. Use: node index.js [${MODES.COLLECT}|${MODES.SPECTATE}]`);
            process.exit(1);
        }
    } catch (error) {
        console.error('Fatal error:', error.message);
        process.exit(1);
    }
}

main();