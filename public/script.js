const jsInput = document.getElementById('js-input');
const cobolOutput = document.getElementById('cobol-output');

let timeout = null;

jsInput.addEventListener('input', () => {
    clearTimeout(timeout);
    timeout = setTimeout(() => {
        transpile(jsInput.value);
    }, 500); // Debounce for 500ms
});

async function transpile(code) {
    if (!code.trim()) {
        cobolOutput.textContent = '';
        return;
    }

    try {
        const response = await fetch('/api/transpile', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ code })
        });

        const data = await response.json();

        if (data.error) {
            cobolOutput.textContent = `Error: ${data.error}`;
            cobolOutput.style.color = '#ef4444'; // Red for error
        } else {
            cobolOutput.textContent = data.cobol;
            cobolOutput.style.color = '#e2e8f0'; // Reset color
        }
    } catch (err) {
        cobolOutput.textContent = `Network Error: ${err.message}`;
        cobolOutput.style.color = '#ef4444';
    }
}

// Initial transpile if there's content (e.g. on reload if browser saves state)
if (jsInput.value) {
    transpile(jsInput.value);
}
