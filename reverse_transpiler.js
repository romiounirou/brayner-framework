class ReverseTranspiler {
    constructor() {
        this.variables = new Map(); // COBOL Name -> JS Name
        this.jsCode = [];
        this.functions = new Map(); // COBOL Section -> JS Function Name
    }

    transpile(cobolCode) {
        const lines = cobolCode.split('\n').map(l => l.trim()).filter(l => l.length > 0);

        // Parse Data Division for variables
        this.parseDataDivision(lines);

        // Parse Procedure Division for logic
        this.parseProcedureDivision(lines);

        return this.jsCode.join('\n');
    }

    parseDataDivision(lines) {
        let inDataDiv = false;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];

            if (line.includes('DATA DIVISION')) {
                inDataDiv = true;
                continue;
            }

            if (line.includes('PROCEDURE DIVISION')) {
                break;
            }

            if (inDataDiv) {
                // Match 77 level variables: 77 VAR-NAME PIC X(100) VALUE "hello".
                const match77 = line.match(/^77\s+([A-Z0-9\-]+)\s+PIC\s+([X9\(\)]+)(?:\s+VALUE\s+(.+))?/i);
                if (match77) {
                    const cobolName = match77[1];
                    const pic = match77[2];
                    let value = match77[3] || null;

                    const jsName = this.toJsVarName(cobolName);
                    this.variables.set(cobolName, jsName);

                    // Determine initial value
                    let initValue = '0';
                    if (value) {
                        value = value.replace(/\.$/, '').trim();
                        if (value.startsWith('"') || value.startsWith("'")) {
                            initValue = value;
                        } else if (value === 'SPACES') {
                            initValue = '""';
                        } else {
                            initValue = value;
                        }
                    } else {
                        initValue = pic.startsWith('X') ? '""' : '0';
                    }

                    this.jsCode.push(`let ${jsName} = ${initValue};`);
                }
            }
        }

        if (this.jsCode.length > 0) {
            this.jsCode.push(''); // Blank line after variables
        }
    }

    parseProcedureDivision(lines) {
        let inProcDiv = false;
        let currentFunction = null;
        let functionBody = [];
        let skipUntilEndIf = 0;
        let skipUntilEndPerform = 0;

        for (let i = 0; i < lines.length; i++) {
            let line = lines[i];

            if (line.includes('PROCEDURE DIVISION')) {
                inProcDiv = true;
                continue;
            }

            if (!inProcDiv) continue;

            // Skip procedure names (e.g., MAIN-PROCEDURE.)
            if (line.match(/^[A-Z0-9\-]+\.$/)) {
                continue;
            }

            // Handle SECTION (functions)
            if (line.includes('SECTION')) {
                if (currentFunction) {
                    // Close previous function
                    this.jsCode.push(`function ${currentFunction}() {`);
                    this.jsCode.push(...functionBody.map(l => '    ' + l));
                    this.jsCode.push('}');
                    this.jsCode.push('');
                }

                const sectionMatch = line.match(/^([A-Z0-9\-]+)\s+SECTION/i);
                if (sectionMatch) {
                    const sectionName = sectionMatch[1];
                    currentFunction = this.toJsFuncName(sectionName);
                    this.functions.set(sectionName, currentFunction);
                    functionBody = [];
                }
                continue;
            }

            // Handle EXIT SECTION
            if (line.includes('EXIT SECTION')) {
                if (currentFunction) {
                    functionBody.push('return;');
                }
                continue;
            }

            // Handle STOP RUN
            if (line.includes('STOP RUN')) {
                this.addStatement('// Program ends', currentFunction ? functionBody : this.jsCode);
                continue;
            }

            // Parse statements
            const statement = this.parseStatement(line, lines, i);
            if (statement) {
                this.addStatement(statement, currentFunction ? functionBody : this.jsCode);
            }
        }

        // Close last function if any
        if (currentFunction) {
            this.jsCode.push(`function ${currentFunction}() {`);
            this.jsCode.push(...functionBody.map(l => '    ' + l));
            this.jsCode.push('}');
        }
    }

    parseStatement(line, allLines, currentIndex) {
        // DISPLAY
        if (line.includes('DISPLAY')) {
            const displayMatch = line.match(/DISPLAY\s+(.+?)\.?$/i);
            if (displayMatch) {
                let value = displayMatch[1].replace(/\.$/, '').trim();
                value = this.resolveValue(value);
                return `console.log(${value});`;
            }
        }

        // MOVE
        if (line.includes('MOVE') && line.includes('TO')) {
            const moveMatch = line.match(/MOVE\s+(.+?)\s+TO\s+(.+?)\.?$/i);
            if (moveMatch) {
                let source = moveMatch[1].trim();
                let target = moveMatch[2].trim();

                source = this.resolveValue(source);
                target = this.resolveVarName(target);

                return `${target} = ${source};`;
            }
        }

        // COMPUTE
        if (line.includes('COMPUTE')) {
            const computeMatch = line.match(/COMPUTE\s+(.+?)\s*=\s*(.+?)\.?$/i);
            if (computeMatch) {
                let target = computeMatch[1].trim();
                let expr = computeMatch[2].trim();

                target = this.resolveVarName(target);
                expr = this.resolveExpression(expr);

                return `${target} = ${expr};`;
            }
        }

        // IF
        if (line.startsWith('IF')) {
            const ifMatch = line.match(/IF\s+(.+?)$/i);
            if (ifMatch) {
                let condition = ifMatch[1].trim();
                condition = this.resolveCondition(condition);
                return `if (${condition}) {`;
            }
        }

        // ELSE
        if (line === 'ELSE') {
            return '} else {';
        }

        // END-IF
        if (line.includes('END-IF')) {
            return '}';
        }

        // PERFORM (function call)
        if (line.startsWith('PERFORM') && !line.includes('UNTIL')) {
            const performMatch = line.match(/PERFORM\s+([A-Z0-9\-]+)\.?$/i);
            if (performMatch) {
                const sectionName = performMatch[1];
                const funcName = this.functions.get(sectionName) || this.toJsFuncName(sectionName);
                return `${funcName}();`;
            }
        }

        // PERFORM UNTIL (while loop)
        if (line.includes('PERFORM UNTIL')) {
            const untilMatch = line.match(/PERFORM\s+UNTIL\s+NOT\s+\((.+?)\)/i);
            if (untilMatch) {
                let condition = untilMatch[1].trim();
                condition = this.resolveCondition(condition);
                return `while (${condition}) {`;
            }
        }

        // END-PERFORM
        if (line.includes('END-PERFORM')) {
            return '}';
        }

        return null;
    }

    resolveValue(value) {
        // String literal
        if (value.startsWith('"') || value.startsWith("'")) {
            return value;
        }

        // Number
        if (!isNaN(value)) {
            return value;
        }

        // Variable
        return this.resolveVarName(value);
    }

    resolveVarName(cobolName) {
        return this.variables.get(cobolName) || cobolName.toLowerCase().replace(/-/g, '_');
    }

    resolveExpression(expr) {
        // Replace COBOL variable names with JS names
        const tokens = expr.split(/\s+/);
        return tokens.map(token => {
            if (token.match(/^[A-Z0-9\-]+$/)) {
                return this.resolveVarName(token);
            }
            return token;
        }).join(' ');
    }

    resolveCondition(condition) {
        // Replace COBOL operators with JS operators
        condition = condition.replace(/\s*=\s*/g, ' === ');
        condition = condition.replace(/NOT\s*=\s*/gi, ' !== ');
        condition = condition.replace(/<>/g, ' !== ');

        // Replace variable names
        const tokens = condition.split(/\s+/);
        return tokens.map(token => {
            if (token.match(/^[A-Z0-9\-]+$/)) {
                return this.resolveVarName(token);
            }
            return token;
        }).join(' ');
    }

    toJsVarName(cobolName) {
        return cobolName.toLowerCase().replace(/-/g, '_');
    }

    toJsFuncName(sectionName) {
        let name = sectionName.toLowerCase().replace(/-/g, '_');
        if (name.startsWith('func_')) {
            name = name.substring(5);
        } else if (name.startsWith('meth_')) {
            name = name.substring(5);
        }
        return name;
    }

    addStatement(statement, target) {
        target.push(statement);
    }
}

module.exports = ReverseTranspiler;
