// utils.js
const fs = require('fs/promises');
const fsSync = require('fs');
const path = require('path');

// API Key Management
async function getApiKey() {
    try {
        const API_KEY = (await fs.readFile('files/api_key.txt', 'utf8')).trim();
        return API_KEY;
    } catch (error) {
        throw new Error('API key not found in files/api_key.txt');
    }
}

// Common delay function
function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

// League Client utility functions
function findLockFile() {
    const possiblePaths = [
        'C:\\Riot Games\\League of Legends',
        'D:\\Riot Games\\League of Legends',
        'C:\\Program Files\\Riot Games\\League of Legends',
        'C:\\Program Files (x86)\\Riot Games\\League of Legends'
    ];

    for (const basePath of possiblePaths) {
        const lockfilePath = path.join(basePath, 'lockfile');
        if (fsSync.existsSync(lockfilePath)) {
            return lockfilePath;
        }
    }
    throw new Error('Lockfile not found');
}

// File operations
async function writeJsonToFile(filename, data) {
    try {
        if (!data || data.length === 0) {
            console.error('No data to write to file');
            return false;
        }
        const jsonData = JSON.stringify({ players: data }, null, 2);
        await fs.writeFile(filename, jsonData);
        return true;
    } catch (error) {
        console.error(`Error saving to ${filename}:`, error.message);
        return false;
    }
}

// List manipulation
function shuffleList(list) {
    return list
        .map(value => ({ value, sort: Math.random() }))
        .sort((a, b) => a.sort - b.sort)
        .map(({ value }) => value);
}

// Console utilities
function updateProgress(current, total, message) {
    process.stdout.clearLine(0);
    process.stdout.cursorTo(0);
    process.stdout.write(`${message}: ${current}/${total}`);
}

// API Headers
function getRiotHeaders(apiKey) {
    return {
        'X-Riot-Token': apiKey,
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36',
        'Accept-Language': 'en-US,en;q=0.9',
        'Accept-Charset': 'application/x-www-form-urlencoded; charset=UTF-8'
    };
}

module.exports = {
    getApiKey,
    delay,
    findLockFile,
    writeJsonToFile,
    shuffleList,
    updateProgress,
    getRiotHeaders
};