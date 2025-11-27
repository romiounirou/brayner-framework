const jsInput = document.getElementById('js-input');
const cobolOutput = document.getElementById('cobol-output');
const swapBtn = document.getElementById('swap-btn');
const modeDisplay = document.getElementById('mode-display');
const jsLabel = document.querySelector('.editor-pane:nth-child(1) .lang-label');
const cobolLabel = document.querySelector('.editor-pane:nth-child(2) .lang-label');

let isReverse = false; // false = JS->COBOL, true = COBOL->JS
let timeout = null;

jsInput.addEventListener('input', () => {
    clearTimeout(timeout);
    timeout = setTimeout(() => {
        transpile(jsInput.value);
    }, 500);
});

swapBtn.addEventListener('click', () => {
    isReverse = !isReverse;

    if (isReverse) {
        modeDisplay.textContent = 'COBOL → JavaScript';
        jsLabel.textContent = 'COBOL';
        cobolLabel.textContent = 'JavaScript';
        jsInput.placeholder = '// Write your COBOL here...\n       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.';
    } else {
        modeDisplay.textContent = 'JavaScript → COBOL';
        jsLabel.textContent = 'JavaScript';
        cobolLabel.textContent = 'COBOL';
        jsInput.placeholder = '// Write your JavaScript here...\nvar msg = \'Hello World\';\nconsole.log(msg);';
    }

    // Clear and re-transpile
    cobolOutput.textContent = '';
    if (jsInput.value.trim()) {
        transpile(jsInput.value);
    }
});

async function transpile(code) {
    if (!code.trim()) {
        cobolOutput.textContent = '';
        return;
    }

    try {
        const endpoint = isReverse ? '/api/reverse' : '/api/transpile';
        const response = await fetch(endpoint, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ code })
        });

        const data = await response.json();

        if (data.error) {
            cobolOutput.textContent = `Error: ${data.error}`;
            cobolOutput.style.color = '#ef4444';
        } else {
            const result = isReverse ? data.javascript : data.cobol;
            cobolOutput.textContent = result;
            cobolOutput.style.color = '#e2e8f0';
        }
    } catch (err) {
        cobolOutput.textContent = `Network Error: ${err.message}`;
        cobolOutput.style.color = '#ef4444';
    }
}

// Initial transpile if there's content
if (jsInput.value) {
    transpile(jsInput.value);
}

// Creator Popup
const logoContainer = document.getElementById('logo-container');
const creatorPopup = document.getElementById('creator-popup');

logoContainer.addEventListener('click', () => {
    creatorPopup.classList.add('active');
});

creatorPopup.addEventListener('click', (e) => {
    if (e.target === creatorPopup) {
        creatorPopup.classList.remove('active');
    }
});

// Copy Button Logic
const copyBtn = document.getElementById('copy-btn');
copyBtn.addEventListener('click', () => {
    const codeContent = cobolOutput.innerText;
    navigator.clipboard.writeText(codeContent).then(() => {
        const originalText = copyBtn.innerHTML;
        copyBtn.innerHTML = `
            <svg width="16" height="16" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path d="M20 6L9 17L4 12" stroke="#4ade80" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            </svg>
            Copied!
        `;
        copyBtn.style.borderColor = '#4ade80';
        copyBtn.style.color = '#4ade80';

        setTimeout(() => {
            copyBtn.innerHTML = originalText;
            copyBtn.style.borderColor = '';
            copyBtn.style.color = '';
        }, 2000);
    }).catch(err => {
        console.error('Failed to copy:', err);
    });
});
