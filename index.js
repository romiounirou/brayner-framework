const fs = require('fs');
const Transpiler = require('./transpiler');
const ReverseTranspiler = require('./reverse_transpiler');
const Executor = require('./executor');

const args = process.argv.slice(2);

if (args.length === 0) {
    console.log('Usage: node index.js <file.js> [--reverse]');
    console.log('  --reverse or -r: Transpile COBOL to JavaScript');
    process.exit(1);
}

const inputFile = args[0];
const isReverse = args.includes('--reverse') || args.includes('-r');

if (!fs.existsSync(inputFile)) {
    console.log(`Error: File ${inputFile} not found.`);
    process.exit(1);
}

const inputCode = fs.readFileSync(inputFile, 'utf-8');

try {
    if (isReverse) {
        // COBOL -> JS
        console.log('[Brayner] Reverse Transpiling', inputFile, '(COBOL -> JS)');

        const reverseTranspiler = new ReverseTranspiler();
        const jsCode = reverseTranspiler.transpile(inputCode);

        const outputFile = inputFile.replace(/\.(cbl|cobol|cob)$/i, '.js');
        fs.writeFileSync(outputFile, jsCode);
        console.log('[Brayner] Generated JavaScript file:', outputFile);
    } else {
        // JS -> COBOL
        console.log('[Brayner] Transpiling', inputFile);

        const transpiler = new Transpiler();
        const cobolCode = transpiler.transpile(inputCode, inputFile);

        const executor = new Executor();
        executor.execute(cobolCode, 'output.cbl');
    }
} catch (err) {
    console.error('[Brayner] Error:', err);
}
