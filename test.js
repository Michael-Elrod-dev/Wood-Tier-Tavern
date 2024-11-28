const { execSync } = require('child_process');

try {
    // Try using WMIC directly
    const wmicOutput = execSync(
        'wmic process where name="LeagueClientUx.exe" get commandline /format:list',
        { encoding: 'utf8' }
    );
    console.log("WMIC Output:", wmicOutput);

    // Also try checking process details
    const processDetails = execSync(
        'powershell.exe -Command "$process = Get-Process LeagueClientUx; $process.StartInfo"',
        { encoding: 'utf8' }
    );
    console.log("Process Details:", processDetails);
} catch (error) {
    console.log("Error:", error.message);
}