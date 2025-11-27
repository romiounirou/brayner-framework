const fs = require('fs');
const Transpiler = require('./transpiler');
const Executor = require('./executor');

const inputFile = process.argv[2];

if (!inputFile) {
    console.log('Usage: node index.js <input_file.js>');
    process.exit(1);
}

if (!fs.existsSync(inputFile)) {
    console.log(`Error: File ${inputFile} not found.`);
    process.exit(1);
}

const code = fs.readFileSync(inputFile, 'utf8');

console.log(`[Brayner] Transpiling ${inputFile}...`);

try {
    const transpiler = new Transpiler();
    const cobolCode = transpiler.transpile(code);

    const executor = new Executor();
    executor.execute(cobolCode, 'output.cbl');

} catch (err) {
    console.error('[Brayner] Error:', err);
}
