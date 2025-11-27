const express = require('express');
const bodyParser = require('body-parser');
const Transpiler = require('./transpiler');
const ReverseTranspiler = require('./reverse_transpiler');

const app = express();
const port = 3000;

app.use(express.static('public'));
app.use(bodyParser.json());

app.post('/api/transpile', (req, res) => {
    const jsCode = req.body.code;
    if (!jsCode) {
        return res.status(400).json({ error: 'No code provided' });
    }

    try {
        const transpiler = new Transpiler();
        const cobolCode = transpiler.transpile(jsCode);
        res.json({ cobol: cobolCode });
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

app.post('/api/reverse', (req, res) => {
    const cobolCode = req.body.code;
    if (!cobolCode) {
        return res.status(400).json({ error: 'No code provided' });
    }

    try {
        const reverseTranspiler = new ReverseTranspiler();
        const jsCode = reverseTranspiler.transpile(cobolCode);
        res.json({ javascript: jsCode });
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

app.listen(port, () => {
    console.log(`Brayner Framework Web Interface running at http://localhost:${port}`);
});
