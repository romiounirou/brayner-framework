const fs = require('fs');
const { spawn } = require('child_process');
const path = require('path');

class Executor {
    constructor() {
    }

    execute(cobolCode, outputFileName = 'program.cbl') {
        const fullPath = path.resolve(outputFileName);
        fs.writeFileSync(fullPath, cobolCode);
        console.log(`[Brayner] COBOL code generated at: ${fullPath}`);

        // Check if cobc exists
        const check = spawn('cobc', ['--version']);

        check.on('error', (err) => {
            console.log('[Brayner] COBOL compiler (cobc) not found. Please install GnuCOBOL to execute the code.');
            console.log('[Brayner] You can manually compile the generated .cbl file.');
        });

        check.on('close', (code) => {
            if (code === 0) {
                console.log('[Brayner] Compiling and executing...');
                this.runCobol(fullPath);
            }
        });
    }

    runCobol(filePath) {
        const exeName = filePath.replace('.cbl', ''); // Windows might need .exe extension handling if cross-compiling, but cobc -x handles it.
        const compile = spawn('cobc', ['-x', '-o', exeName, filePath]);

        compile.stderr.on('data', (data) => {
            console.error(`[Compiler Error]: ${data}`);
        });

        compile.on('close', (code) => {
            if (code === 0) {
                console.log('[Brayner] Execution output:');
                console.log('--------------------------------------------------');

                // On Windows, we might need to append .exe
                let runCmd = exeName;
                if (process.platform === 'win32' && !runCmd.endsWith('.exe')) {
                    runCmd += '.exe';
                }

                const run = spawn(runCmd, [], { stdio: 'inherit' });

                run.on('close', (runCode) => {
                    console.log('--------------------------------------------------');
                    console.log(`[Brayner] Program finished with code ${runCode}`);
                });
            } else {
                console.log('[Brayner] Compilation failed.');
            }
        });
    }
}

module.exports = Executor;
