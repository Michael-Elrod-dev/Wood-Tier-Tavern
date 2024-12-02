const mongoose = require('mongoose');

// User Schema
const userSchema = new mongoose.Schema({
    username: { type: String, required: true, unique: true },
    userType: { 
        type: String, 
        enum: ['viewer', 'subscriber', 'vip', 'moderator'],
        default: 'viewer'
    },
    balance: { type: Number, default: 1000 },
    totalBets: { type: Number, default: 0 },
    wins: { type: Number, default: 0 },
    losses: { type: Number, default: 0 },
    highestWin: { type: Number, default: 0 },
    biggestBet: { type: Number, default: 0 },
    currentStreak: { type: Number, default: 0 },
    longestWinStreak: { type: Number, default: 0 },
    totalWinnings: { type: Number, default: 0 },
    rank: { 
        type: String, 
        enum: [
            'Bronze Bettor',
            'Silver Gambler',
            'Gold Wagerer',
            'Platinum Punter',
            'Diamond Dealer',
            'Master Gamester',
            'Grandmaster Bookie',
            'Legendary Better'
        ],
        default: 'Bronze Bettor'},
    achievements: [{
        name: String,
        count: { type: Number, default: 1 },
        firstEarnedAt: Date,
        lastEarnedAt: Date
    }],
    lastBet: {
        amount: Number,
        team: String,
        gameId: String
    },
    createdAt: { type: Date, default: Date.now },
    lastActive: { type: Date, default: Date.now }
});

// Game Schema
const gameSchema = new mongoose.Schema({
    status: { 
        type: String, 
        enum: ['open', 'locked', 'inProgress', 'completed'], 
        default: 'open'
    },
    blueTeam: {
        totalBets: { type: Number, default: 0 },
        numberOfBettors: { type: Number, default: 0 }
    },
    redTeam: {
        totalBets: { type: Number, default: 0 },
        numberOfBettors: { type: Number, default: 0 }
    },
    winner: { type: String, enum: ['blue', 'red', null], default: null },
    startTime: { type: Date, default: Date.now },
    endTime: Date,
    bets: [{
        username: String,
        amount: Number,
        team: String,
        timestamp: { type: Date, default: Date.now }
    }]
});

// Create models
const User = mongoose.model('User', userSchema);
const Game = mongoose.model('Game', gameSchema);

module.exports = { User, Game };