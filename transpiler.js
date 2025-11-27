const acorn = require('acorn');

class Transpiler {
    constructor() {
        this.variables = new Map(); // JS Name -> COBOL Name
        this.variableTypes = new Map(); // JS Name -> Type (string, number, array)
        this.workingStorage = [];
        this.mainProcedure = [];
        this.sections = [];
        this.currentBuffer = this.mainProcedure; // Where we are currently writing code
        this.labelCount = 0;
        this.functions = new Map(); // Func Name -> COBOL Section Name
    }

    transpile(code) {
        const ast = acorn.parse(code, { ecmaVersion: 2020 });

        // Header
        this.workingStorage.push('       IDENTIFICATION DIVISION.');
        this.workingStorage.push('       PROGRAM-ID. BRAYNER-APP.');
        this.workingStorage.push('       DATA DIVISION.');
        this.workingStorage.push('       WORKING-STORAGE SECTION.');

        // Default variables
        this.addVariable('WS-TEMP-NUM', '9(9) VALUE 0');
        this.addVariable('WS-TEMP-STR', 'X(100) VALUE SPACES');
        this.addVariable('WS-RETURN-VAL', '9(9) VALUE 0'); // Simple return value register

        // First pass: Hoist functions and variables
        this.hoist(ast);

        this.traverse(ast);

        // Finalize Main Procedure
        this.mainProcedure.push('           STOP RUN.');

        // Combine all parts
        const wsCode = Array.from(this.variables.values()).map(v => v.def).join('\n');

        return [
            ...this.workingStorage,
            wsCode,
            '       PROCEDURE DIVISION.',
            '       MAIN-PROCEDURE.',
            ...this.mainProcedure,
            ...this.sections
        ].join('\n');
    }

    addVariable(name, picture, isArray = false) {
        if (!this.variables.has(name)) {
            let cobolName = name.toUpperCase().replace(/_/g, '-').replace(/([a-z])([A-Z])/g, '$1-$2').toUpperCase();
            // Ensure unique COBOL names if needed, but for now assume 1:1
            if (name.startsWith('WS-')) cobolName = name; // Internal vars

            const def = `       77  ${cobolName} PIC ${picture}.`;
            this.variables.set(name, { name: cobolName, def });
            return cobolName;
        }
        return this.variables.get(name).name;
    }

    addArray(name, size = 100) {
        if (!this.variables.has(name)) {
            const cobolName = name.toUpperCase().replace(/_/g, '-').toUpperCase();
            // COBOL tables: 01 TABLE-NAME. 05 ELEMENT PIC X(100) OCCURS 100 TIMES.
            // Simplified: We'll just use a flat definition here for simplicity or a specific structure
            // But 77 cannot have OCCURS. Must be 01 level.
            // We will inject 01 levels into workingStorage directly or handle differently.
            // For this demo, let's use a specific hack: 
            // We'll append to a specific list of tables.

            const def = `       01  ${cobolName}-TABLE.\n           05 ${cobolName} PIC 9(9) OCCURS ${size} TIMES.`;
            this.variables.set(name, { name: `${cobolName}`, def, isArray: true });
            return cobolName;
        }
        return this.variables.get(name).name;
    }

    hoist(node) {
        // Simple hoisting for function declarations
        if (node.type === 'Program') {
            node.body.forEach(child => {
                if (child.type === 'FunctionDeclaration') {
                    const funcName = child.id.name;
                    const sectionName = `FUNC-${funcName.toUpperCase()}`;
                    this.functions.set(funcName, sectionName);
                }
            });
        }
    }

    addCode(line) {
        this.currentBuffer.push(`           ${line}`);
    }

    traverse(node) {
        if (!node) return;

        switch (node.type) {
            case 'Program':
            case 'BlockStatement':
                node.body.forEach(child => this.traverse(child));
                break;

            case 'VariableDeclaration':
                node.declarations.forEach(decl => {
                    const varName = decl.id.name;
                    let initVal = null;

                    if (decl.init) {
                        if (decl.init.type === 'ArrayExpression') {
                            this.addArray(varName);
                            // Initialize array elements if provided
                            decl.init.elements.forEach((el, index) => {
                                if (el.type === 'Literal') {
                                    const cobolVar = this.variables.get(varName).name;
                                    this.addCode(`MOVE ${el.value} TO ${cobolVar} (${index + 1}).`);
                                }
                            });
                            return;
                        }
                        initVal = this.resolveValue(decl.init);
                    }

                    // Guess type based on init or default to Number (9) or String (X)
                    // For now, default everything to Number if not string literal
                    let pic = '9(9)';
                    if (decl.init && decl.init.type === 'Literal' && typeof decl.init.value === 'string') {
                        pic = 'X(100)';
                    }

                    const cobolVar = this.addVariable(varName, pic);
                    if (initVal !== null) {
                        this.addCode(`MOVE ${initVal} TO ${cobolVar}.`);
                    }
                });
                break;

            case 'FunctionDeclaration':
                // Switch buffer to sections
                const oldBuffer = this.currentBuffer;
                this.currentBuffer = this.sections;

                const funcName = node.id.name;
                const sectionName = this.functions.get(funcName);

                this.sections.push(`       ${sectionName} SECTION.`);

                // Handle parameters (simplified: assume global vars or map params to globals)
                node.params.forEach((param, index) => {
                    // In a real compiler, we'd pop from stack. Here, we assume caller set specific globals?
                    // Or we just treat params as local variables that were set before call.
                    // Let's just register them as variables.
                    this.addVariable(param.name, '9(9)');
                });

                this.traverse(node.body);

                this.addCode('EXIT SECTION.');
                this.currentBuffer = oldBuffer;
                break;

            case 'ReturnStatement':
                if (node.argument) {
                    const val = this.resolveValue(node.argument);
                    this.addCode(`MOVE ${val} TO WS-RETURN-VAL.`);
                }
                this.addCode('EXIT SECTION.'); // This might be premature if inside IF, but COBOL GO TO is needed for proper return.
                // For simplicity, we assume return is at end or we just set value.
                break;

            case 'ExpressionStatement':
                this.traverse(node.expression);
                break;

            case 'CallExpression':
                if (node.callee.type === 'MemberExpression' &&
                    node.callee.object.name === 'console' &&
                    node.callee.property.name === 'log') {
                    node.arguments.forEach(arg => {
                        const val = this.resolveValue(arg);
                        this.addCode(`DISPLAY ${val}.`);
                    });
                } else if (node.callee.type === 'Identifier') {
                    // User function call
                    const funcName = node.callee.name;
                    const sectionName = this.functions.get(funcName);
                    if (sectionName) {
                        // Pass arguments (simplified: MOVE to param vars)
                        // We need to know param names. We don't have them here easily without symbol table.
                        // LIMITATION: Arguments not fully supported in this simple version without symbol table lookup of func def.
                        this.addCode(`PERFORM ${sectionName}.`);
                    }
                }
                break;

            case 'AssignmentExpression':
                if (node.left.type === 'Identifier') {
                    const targetVar = this.variables.get(node.left.name)?.name || this.addVariable(node.left.name, '9(9)');
                    const val = this.resolveExpression(node.right, targetVar);
                    if (val) this.addCode(`MOVE ${val} TO ${targetVar}.`);
                } else if (node.left.type === 'MemberExpression') {
                    // Array assignment: arr[i] = x
                    const arrName = node.left.object.name;
                    const index = this.resolveValue(node.left.property); // Assuming index is simple
                    const arrVar = this.variables.get(arrName);
                    if (arrVar && arrVar.isArray) {
                        // COBOL is 1-based. If index is literal number, add 1. If variable, need COMPUTE.
                        let finalIndex = index;
                        if (!isNaN(index)) {
                            finalIndex = parseInt(index) + 1;
                        } else {
                            // It's a variable. We need to compute index + 1.
                            this.addCode(`COMPUTE WS-TEMP-NUM = ${index} + 1.`);
                            finalIndex = 'WS-TEMP-NUM';
                        }

                        const val = this.resolveValue(node.right);
                        this.addCode(`MOVE ${val} TO ${arrVar.name} (${finalIndex}).`);
                    }
                }
                break;

            case 'IfStatement':
                const condition = this.resolveCondition(node.test);
                this.addCode(`IF ${condition}`);
                this.traverse(node.consequent);
                if (node.alternate) {
                    this.addCode('ELSE');
                    this.traverse(node.alternate);
                }
                this.addCode('END-IF.');
                break;

            case 'WhileStatement':
                const whileCond = this.resolveCondition(node.test);
                // COBOL: PERFORM UNTIL NOT (condition)
                // Actually PERFORM UNTIL condition-is-false is standard, so "PERFORM UNTIL NOT (cond)"
                this.addCode(`PERFORM UNTIL NOT (${whileCond})`);
                this.traverse(node.body);
                this.addCode('END-PERFORM.');
                break;

            case 'ForStatement':
                // for (init; test; update) body
                // Map to PERFORM ... UNTIL ...
                // We need to handle init, then loop.
                if (node.init) this.traverse(node.init); // var i = 0;

                const forCond = this.resolveCondition(node.test);
                this.addCode(`PERFORM UNTIL NOT (${forCond})`);
                this.traverse(node.body);
                if (node.update) this.traverse(node.update); // i++
                this.addCode('END-PERFORM.');
                break;
        }
    }

    resolveExpression(node, targetVar) {
        if (node.type === 'BinaryExpression') {
            this.handleBinaryExpression(node, targetVar);
            return null; // Handled internally
        }
        return this.resolveValue(node);
    }

    handleBinaryExpression(node, targetVar) {
        let left = this.resolveValue(node.left);
        let right = this.resolveValue(node.right);

        let op = '';
        if (node.operator === '+') op = '+';
        else if (node.operator === '-') op = '-';
        else if (node.operator === '*') op = '*';
        else if (node.operator === '/') op = '/';

        if (op) {
            this.addCode(`COMPUTE ${targetVar} = ${left} ${op} ${right}.`);
        }
    }

    resolveValue(node) {
        if (node.type === 'Literal') {
            return typeof node.value === 'string' ? `"${node.value}"` : node.value;
        } else if (node.type === 'Identifier') {
            return this.variables.get(node.name)?.name || node.name; // Fallback to name if not found (might be loop var)
        } else if (node.type === 'MemberExpression') {
            // Array access: arr[i]
            const arrName = node.object.name;
            const index = this.resolveValue(node.property);
            const arrVar = this.variables.get(arrName);
            if (arrVar && arrVar.isArray) {
                if (!isNaN(index)) {
                    return `${arrVar.name} (${parseInt(index) + 1})`;
                } else {
                    // Complex index not fully supported inline without temp var, 
                    // but COBOL allows variable index: ARR (IDX)
                    // But we need IDX+1.
                    // For simplicity, assume user handles 1-based or we ignore 0-based adjustment for vars in this simple version
                    // OR we use the temp var hack again but that's risky in expression.
                    return `${arrVar.name} (${index})`;
                }
            }
        }
        return '0';
    }

    resolveCondition(node) {
        if (node.type === 'BinaryExpression') {
            const left = this.resolveValue(node.left);
            const right = this.resolveValue(node.right);
            let op = node.operator;
            if (op === '===') op = '=';
            if (op === '!==') op = 'NOT =';
            return `${left} ${op} ${right}`;
        }
        return '1 = 1'; // Default true
    }
}

module.exports = Transpiler;
