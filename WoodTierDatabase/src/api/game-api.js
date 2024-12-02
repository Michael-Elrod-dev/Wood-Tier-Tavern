const { Game, User } = require('../schemas');

// Create a new game
async function createGame() {
    const game = new Game({
        status: 'open',
        blueTeam: { totalBets: 0, numberOfBettors: 0 },
        redTeam: { totalBets: 0, numberOfBettors: 0 },
        startTime: new Date()
    });
    return await game.save();
}

// Get current active game
async function getActiveGame() {
    const game = await Game.findOne({ status: { $in: ['open', 'locked', 'inProgress'] } });
    if (!game) throw new Error('No active game found');
    return game;
}

// Place bet on active game
async function placeBet(username, team, amount) {
    const game = await getActiveGame();
    if (game.status !== 'open') {
        throw new Error('Betting is closed for this game');
    }

    const user = await User.findOne({ username });
    if (!user) throw new Error('User not found');
    if (user.balance < amount) throw new Error('Insufficient balance');

    // Update game with new bet
    const updateData = {
        $inc: {
            [`${team}Team.totalBets`]: amount,
            [`${team}Team.numberOfBettors`]: 1
        },
        $push: {
            bets: {
                username,
                amount,
                team,
                timestamp: new Date()
            }
        }
    };

    // Update game and user atomically
    const [updatedGame] = await Promise.all([
        Game.findByIdAndUpdate(game._id, updateData, { new: true }),
        User.findOneAndUpdate(
            { username },
            { 
                $inc: { balance: -amount },
                $set: { 
                    lastBet: {
                        amount,
                        team,
                        gameId: game._id
                    }
                }
            }
        )
    ]);

    return updatedGame;
}

// Update game status
async function updateGameStatus(gameId, newStatus) {
    const game = await Game.findByIdAndUpdate(
        gameId,
        { 
            status: newStatus,
            ...(newStatus === 'completed' ? { endTime: new Date() } : {})
        },
        { new: true }
    );
    if (!game) throw new Error('Game not found');
    return game;
}

// End game and process payouts
async function endGame(gameId, winningTeam) {
    const game = await Game.findById(gameId);
    if (!game) throw new Error('Game not found');
    if (game.status === 'completed') throw new Error('Game already completed');

    // Calculate payouts
    const totalPot = game.blueTeam.totalBets + game.redTeam.totalBets;
    const winningPot = game[`${winningTeam}Team`].totalBets;
    
    // Process all winning bets
    const winningBets = game.bets.filter(bet => bet.team === winningTeam);
    for (const bet of winningBets) {
        const payout = (bet.amount / winningPot) * totalPot;
        await User.findOneAndUpdate(
            { username: bet.username },
            { $inc: { balance: payout } }
        );
    }

    // Update game status
    game.status = 'completed';
    game.winner = winningTeam;
    game.endTime = new Date();
    await game.save();

    // Create new game automatically
    await createGame();

    return game;
}

// Get game statistics
async function getGameStats(gameId) {
    const game = await Game.findById(gameId);
    if (!game) throw new Error('Game not found');

    return {
        status: game.status,
        totalBets: game.blueTeam.totalBets + game.redTeam.totalBets,
        blueTeam: {
            totalBets: game.blueTeam.totalBets,
            numberOfBettors: game.blueTeam.numberOfBettors
        },
        redTeam: {
            totalBets: game.redTeam.totalBets,
            numberOfBettors: game.redTeam.numberOfBettors
        },
        winner: game.winner,
        duration: game.endTime ? (game.endTime - game.startTime) / 1000 : null, // in seconds
        betsCount: game.bets.length
    };
}

// Get recent games history
async function getRecentGames(limit = 10) {
    return await Game.find({ status: 'completed' })
        .sort({ endTime: -1 })
        .limit(limit)
        .select('winner blueTeam redTeam startTime endTime');
}

module.exports = {
    createGame,
    getActiveGame,
    placeBet,
    updateGameStatus,
    endGame,
    getGameStats,
    getRecentGames
};