// utils.js
const path = require('path');
const fsSync = require('fs');
const axios = require('axios');
const https = require('https');
const fs = require('fs/promises');

async function getApiKey() {
    try {
        const API_KEY = (await fs.readFile('../files/api_key.txt', 'utf8')).trim();
        return API_KEY;
    } catch (error) {
        throw new Error('API key not found in ../files/api_key.txt');
    }
}

function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function findLockFile() {
    const possiblePaths = [
        'C:\\Riot Games\\League of Legends',
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

async function readPlayerFile(filename) {
    try {
        const content = await fs.readFile(filename, 'utf8');
        const jsonData = JSON.parse(content);

        return jsonData.players
            .filter(player =>
                player.summonerId &&
                player.puuid &&
                player.gameName &&
                player.tagLine
            );
    } catch (error) {
        console.error(`Error reading ${filename}:`, error);
        return [];
    }
}

function shuffleList(list) {
    return list
        .map(value => ({ value, sort: Math.random() }))
        .sort((a, b) => a.sort - b.sort)
        .map(({ value }) => value);
}

function updateProgress(current, remaining, message) {
    process.stdout.clearLine(0);
    process.stdout.cursorTo(0);
    process.stdout.write(`${message}: ${current}/${remaining}`);
}

async function makeRiotRequest(apiKey, url, method = 'GET', payload = null) {
    const config = {
        method,
        url,
        headers: {
            'X-Riot-Token': apiKey,
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36',
            'Accept-Language': 'en-US,en;q=0.9',
            'Accept-Charset': 'application/x-www-form-urlencoded; charset=UTF-8'
        },
        data: payload
    };

    try {
        const response = await axios(config);
        return response.data;
    } catch (error) {
        console.log('API error:', {
            status: error.response?.status,
            statusText: error.response?.statusText,
            data: error.response?.data
        });
        throw error;
    }
}

async function makeLcuRequest(credentials, method, endpoint, payload = null) {
    const url = `https://127.0.0.1:${credentials.port}${endpoint}`;
    const auth = Buffer.from(`riot:${credentials.auth}`).toString('base64');
    
    // console.log(`Making request to: ${url}`);
    // console.log('Using auth token:', auth);
    
    const config = {
        method,
        url,
        headers: {
            'Authorization': `Basic ${auth}`,
            'Content-Type': 'application/json'
        },
        httpsAgent: new https.Agent({
            rejectUnauthorized: false
        }),
        data: payload
    };

    try {
        const response = await axios(config);
        return response.data;
    } catch (error) {
        console.log('Full error:', {
            status: error.response?.status,
            statusText: error.response?.statusText,
            data: error.response?.data,
            message: error.message
        });
        throw new Error(`LCU request failed: ${error.response?.status} ${error.response?.statusText}`);
    }
}

module.exports = {
    getApiKey,
    delay,
    findLockFile,
    writeJsonToFile,
    readPlayerFile,
    shuffleList,
    updateProgress,
    makeRiotRequest,
    makeLcuRequest
};