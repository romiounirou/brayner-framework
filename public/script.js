document.addEventListener('DOMContentLoaded', () => {
    const jsInput = document.getElementById('js-input');
    const cobolOutput = document.getElementById('cobol-output');
    const swapBtn = document.getElementById('swap-btn');
    const modeDisplay = document.getElementById('mode-display');
    const jsLabel = document.querySelector('.editor-pane:nth-child(1) .lang-label');
    const cobolLabel = document.querySelector('.editor-pane:nth-child(2) .lang-label');

    let isReverse = false;
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
            jsInput.placeholder = '       IDENTIFICATION DIVISION.\\n       PROGRAM-ID. TEST.\\n       PROCEDURE DIVISION.\\n       MAIN-PROCEDURE.\\n           DISPLAY "Hello".\\n           STOP RUN.';
        } else {
            modeDisplay.textContent = 'JavaScript → COBOL';
            jsLabel.textContent = 'JavaScript';
            cobolLabel.textContent = 'COBOL';
            jsInput.placeholder = '// Write your JavaScript here...\\nvar msg = \\'Hello World\\';\\nconsole.log(msg);';
        }

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

    if (jsInput.value) {
        transpile(jsInput.value);
    }

    // Creator Popup
    const logoContainer = document.getElementById('logo-container');
    const creatorPopup = document.getElementById('creator-popup');

    if (logoContainer && creatorPopup) {
        logoContainer.addEventListener('click', () => {
            console.log('Logo clicked!');
            creatorPopup.classList.add('active');
        });

        creatorPopup.addEventListener('click', (e) => {
            if (e.target === creatorPopup) {
                creatorPopup.classList.remove('active');
            }
        });
    }

    // Copy Buttons
    const copyInputBtn = document.getElementById('copy-input-btn');
    const copyOutputBtn = document.getElementById('copy-output-btn');

    async function copyToClipboard(text, button) {
        try {
            await navigator.clipboard.writeText(text);

            button.classList.add('copied');
            const originalHTML = button.innerHTML;
            button.innerHTML = `
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <polyline points="20 6 9 17 4 12"></polyline>
                </svg>
                Copied!
            `;

            setTimeout(() => {
                button.classList.remove('copied');
                button.innerHTML = originalHTML;
            }, 2000);
        } catch (err) {
            console.error('Failed to copy:', err);
        }
    }

    if (copyInputBtn) {
        copyInputBtn.addEventListener('click', () => {
            copyToClipboard(jsInput.value, copyInputBtn);
        });
    }

    if (copyOutputBtn) {
        copyOutputBtn.addEventListener('click', () => {
            copyToClipboard(cobolOutput.textContent, copyOutputBtn);
        });
    }
});
