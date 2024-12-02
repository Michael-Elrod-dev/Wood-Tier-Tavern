const tmi = require('tmi.js');
const TokenManager = require('./tokenManager');

const tokenManager = new TokenManager();
const client = new tmi.client(tokenManager.getConfig());

client.on('message', onMessageHandler);
client.on('connected', onConnectedHandler);
client.on('disconnected', async (reason) => {
    console.log(`* Bot disconnected: ${reason}`);
    try {
        await tokenManager.refreshToken();
        client.connect();
    } catch (error) {
        console.error('Failed to refresh token:', error);
    }
});

client.connect();

async function onMessageHandler (target, context, msg, self) {
    if (self) { return; }
    const commandName = msg.trim().toLowerCase();

    if (commandName === 'hello') {
        client.say(target, 'Kappa');
        console.log(`* Executed hello command`);
    }
}

function onConnectedHandler (addr, port) {
    console.log(`* Connected to ${addr}:${port}`);
}