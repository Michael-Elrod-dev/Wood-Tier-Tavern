const mongoose = require('mongoose');
const userApi = require('./api/user-api');
const gameApi = require('./api/game-api');
const { User, Game } = require('./schemas');
const dbURI = 'mongodb://localhost:27017/WoodTierDatabase';

// Function to clear database
async function clearDatabase() {
    console.log('Clearing database...');
    await User.deleteMany({});
    await Game.deleteMany({});
    console.log('Database cleared');
}

mongoose.connect(dbURI)
    .then(async () => {
        console.log('Connected to MongoDB');
        await clearDatabase();
        await testUserAndGame();
        await clearDatabase();
        await mongoose.connection.close();
        console.log('Tests completed and database cleaned');
        process.exit(0);
    })
    .catch((err) => {
        console.error('MongoDB connection error:', err);
        process.exit(1);
    });

mongoose.connection.on('error', err => {
    console.error('MongoDB error:', err);
});

mongoose.connection.on('disconnected', () => {
    console.log('MongoDB disconnected');
});

// Test function
async function testUserAndGame() {
    try {
        // Test user operations
        const newUser = await userApi.createUser('testUser');
        console.log('Created user:', newUser);

        // Test game operations
        const newGame = await gameApi.createGame();
        console.log('Created game:', newGame);

        // Test placing bet
        await gameApi.placeBet('testUser', 'blue', 100);
        console.log('Placed bet successfully');

        // Get user stats
        const stats = await userApi.getUserStats('testUser');
        console.log('User stats:', stats);

    } catch (error) {
        console.error('Test error:', error.message);
        process.exit(1);
    }
}

process.on('SIGINT', async () => {
    await mongoose.connection.close();
    process.exit(0);
});

module.exports = mongoose.connection;