const { User } = require('../schemas');

// Create new user
async function createUser(username, userType = 'viewer') {
    try {
        const user = new User({ username, userType });
        return await user.save();
    } catch (error) {
        if (error.code === 11000) {
            throw new Error('Username already exists');
        }
        throw error;
    }
}

// Get user by username
async function getUser(username) {
    const user = await User.findOne({ username });
    if (!user) throw new Error('User not found');
    return user;
}

// Update user balance
async function updateBalance(username, amount) {
    const user = await User.findOneAndUpdate(
        { username },
        { 
            $inc: { balance: amount },
            $set: { lastActive: new Date() }
        },
        { new: true }
    );
    if (!user) throw new Error('User not found');
    return user;
}

// Update user type (for when users sub/unsub)
async function updateUserType(username, newType) {
    const user = await User.findOneAndUpdate(
        { username },
        { 
            userType: newType,
            lastActive: new Date()
        },
        { new: true }
    );
    if (!user) throw new Error('User not found');
    return user;
}

// Add achievement
async function addAchievement(username, achievementName) {
    const user = await User.findOne({ username });
    if (!user) throw new Error('User not found');

    const existingAchievement = user.achievements.find(a => a.name === achievementName);
    
    if (existingAchievement) {
        // Update existing achievement
        existingAchievement.count += 1;
        existingAchievement.lastEarnedAt = new Date();
    } else {
        // Add new achievement
        user.achievements.push({
            name: achievementName,
            count: 1,
            firstEarnedAt: new Date(),
            lastEarnedAt: new Date()
        });
    }

    return await user.save();
}

// Update rank
async function updateRank(username, newRank) {
    const user = await User.findOneAndUpdate(
        { username },
        { 
            rank: newRank,
            lastActive: new Date()
        },
        { new: true }
    );
    if (!user) throw new Error('User not found');
    return user;
}

// Update betting stats after a game
async function updateUserStats(username, betAmount, didWin) {
    const updateData = {
        $inc: {
            totalBets: 1,
            [didWin ? 'wins' : 'losses']: 1,
            totalWinnings: didWin ? betAmount : -betAmount
        },
        $set: { lastActive: new Date() }
    };

    // Update streak
    if (didWin) {
        updateData.$inc.currentStreak = 1;
    } else {
        updateData.$set.currentStreak = 0;
    }

    const user = await User.findOne({ username });
    if (!user) throw new Error('User not found');

    // Update highest win if applicable
    if (didWin && betAmount > user.highestWin) {
        updateData.$set.highestWin = betAmount;
    }

    // Update biggest bet if applicable
    if (betAmount > user.biggestBet) {
        updateData.$set.biggestBet = betAmount;
    }

    // Update longest win streak if applicable
    if (didWin && user.currentStreak + 1 > user.longestWinStreak) {
        updateData.$set.longestWinStreak = user.currentStreak + 1;
    }

    return await User.findOneAndUpdate(
        { username },
        updateData,
        { new: true }
    );
}

// Get user's betting stats
async function getUserStats(username) {
    const user = await User.findOne({ username });
    if (!user) throw new Error('User not found');

    return {
        username: user.username,
        userType: user.userType,
        balance: user.balance,
        rank: user.rank,
        stats: {
            totalBets: user.totalBets,
            wins: user.wins,
            losses: user.losses,
            winRate: user.totalBets ? (user.wins / user.totalBets * 100).toFixed(2) + '%' : '0%',
            highestWin: user.highestWin,
            biggestBet: user.biggestBet,
            currentStreak: user.currentStreak,
            longestWinStreak: user.longestWinStreak,
            totalWinnings: user.totalWinnings
        },
        achievements: user.achievements
    };
}

// Get leaderboard
async function getLeaderboard(sortBy = 'balance', limit = 10) {
    const sortOptions = {
        balance: { balance: -1 },
        winRate: { wins: -1 },
        totalWinnings: { totalWinnings: -1 }
    };

    return await User.find()
        .sort(sortOptions[sortBy])
        .limit(limit)
        .select('username balance wins losses totalWinnings rank');
}

module.exports = {
    createUser,
    getUser,
    updateBalance,
    updateUserType,
    addAchievement,
    updateRank,
    updateUserStats,
    getUserStats,
    getLeaderboard
};