const parser = require('@babel/parser');
const traverse = require('@babel/traverse').default;
const t = require('@babel/types');

class Transpiler {
    constructor() {
        this.variables = new Map(); // JS Name -> COBOL Name
        this.workingStorage = [];
        this.mainProcedure = [];
        this.sections = [];
        this.currentBuffer = this.mainProcedure;
        this.functions = new Map(); // Func Name -> COBOL Section Name
        this.labelCount = 0;
    }

    transpile(code, filename = 'input.js') {
        // Pre-process Vue files
        if (code.trim().startsWith('<template') || code.trim().startsWith('<script') || code.includes('</script>')) {
            const scriptMatch = code.match(/<script[^>]*>([\s\S]*?)<\/script>/);
            if (scriptMatch) {
                code = scriptMatch[1];
            } else if (code.trim().startsWith('<template')) {
                // Vue file without script or failed to match
                // Try to parse as is, but if it fails, it's likely because we couldn't extract script
            }
        }

        let ast;
        try {
            ast = parser.parse(code, {
                sourceType: 'module',
                plugins: ['typescript', 'jsx', 'classProperties', 'decorators-legacy']
            });
        } catch (err) {
            throw new Error(`Syntax Error: ${err.message}. Ensure you are using valid JavaScript, TypeScript, React, or Vue syntax.`);
        }

        // Header
        this.workingStorage.push('       IDENTIFICATION DIVISION.');
        this.workingStorage.push('       PROGRAM-ID. BRAYNER-APP.');
        this.workingStorage.push('       DATA DIVISION.');
        this.workingStorage.push('       WORKING-STORAGE SECTION.');

        // Default variables
        this.addVariable('WS-TEMP-NUM', '9(9) VALUE 0');
        this.addVariable('WS-TEMP-STR', 'X(100) VALUE SPACES');
        this.addVariable('WS-RETURN-VAL', '9(9) VALUE 0');

        // First pass: Hoist functions
        this.hoist(ast);

        // Traverse using Babel traverse
        traverse(ast, {
            enter: (path) => this.visit(path)
        });

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
            // Sanitize
            cobolName = cobolName.replace(/[^A-Z0-9\-]/g, '');
            if (!cobolName) cobolName = 'VAR-' + this.labelCount++;

            const def = `       77  ${cobolName} PIC ${picture}.`;
            this.variables.set(name, { name: cobolName, def });
            return cobolName;
        }
        return this.variables.get(name).name;
    }

    addArray(name, size = 100) {
        if (!this.variables.has(name)) {
            const cobolName = name.toUpperCase().replace(/_/g, '-').toUpperCase();
            const def = `       01  ${cobolName}-TABLE.\n           05 ${cobolName} PIC 9(9) OCCURS ${size} TIMES.`;
            this.variables.set(name, { name: `${cobolName}`, def, isArray: true });
            return cobolName;
        }
        return this.variables.get(name).name;
    }

    hoist(ast) {
        traverse(ast, {
            FunctionDeclaration: (path) => {
                const funcName = path.node.id.name;
                const sectionName = `FUNC-${funcName.toUpperCase()}`;
                this.functions.set(funcName, sectionName);
            },
            // Also hoist methods in classes (React/Angular)
            ClassMethod: (path) => {
                if (path.node.key.type === 'Identifier') {
                    const funcName = path.node.key.name;
                    const sectionName = `METH-${funcName.toUpperCase()}`;
                    this.functions.set(funcName, sectionName);
                }
            }
        });
    }

    addCode(line) {
        this.currentBuffer.push(`           ${line}`);
    }

    visit(path) {
        const { node } = path;

        // Skip TS Types
        if (t.isTSTypeAliasDeclaration(node) || t.isTSInterfaceDeclaration(node)) {
            path.skip();
            return;
        }

        if (t.isVariableDeclaration(node)) {
            node.declarations.forEach(decl => {
                if (!t.isIdentifier(decl.id)) return;
                const varName = decl.id.name;
                let initVal = null;

                if (decl.init) {
                    if (t.isArrayExpression(decl.init)) {
                        this.addArray(varName);
                        decl.init.elements.forEach((el, index) => {
                            if (t.isNumericLiteral(el)) {
                                const cobolVar = this.variables.get(varName).name;
                                this.addCode(`MOVE ${el.value} TO ${cobolVar} (${index + 1}).`);
                            }
                        });
                        return;
                    }
                    initVal = this.resolveValue(decl.init);
                }

                let pic = '9(9)';
                if (decl.init && t.isStringLiteral(decl.init)) {
                    pic = 'X(100)';
                }

                const cobolVar = this.addVariable(varName, pic);
                if (initVal !== null) {
                    this.addCode(`MOVE ${initVal} TO ${cobolVar}.`);
                }
            });
            path.skip();
        }

        if (t.isFunctionDeclaration(node) || t.isClassMethod(node)) {
            const oldBuffer = this.currentBuffer;
            this.currentBuffer = this.sections;

            let funcName = '';
            if (t.isFunctionDeclaration(node)) {
                funcName = node.id.name;
            } else if (t.isClassMethod(node)) {
                if (t.isIdentifier(node.key)) {
                    funcName = node.key.name;
                }
            }

            if (funcName) {
                let sectionName = this.functions.get(funcName);
                if (!sectionName) {
                    sectionName = `FUNC-${funcName.toUpperCase()}`;
                    this.functions.set(funcName, sectionName);
                }

                this.sections.push(`       ${sectionName} SECTION.`);

                node.params.forEach(param => {
                    if (t.isIdentifier(param)) {
                        this.addVariable(param.name, '9(9)');
                    }
                });

                path.get('body').traverse({
                    enter: (p) => this.visit(p)
                });

                this.addCode('EXIT SECTION.');
            }
            this.currentBuffer = oldBuffer;
            path.skip();
        }

        if (t.isReturnStatement(node)) {
            if (node.argument) {
                const val = this.resolveValue(node.argument);
                this.addCode(`MOVE ${val} TO WS-RETURN-VAL.`);
            }
            this.addCode('EXIT SECTION.');
            path.skip();
        }

        if (t.isCallExpression(node)) {
            if (t.isMemberExpression(node.callee) &&
                t.isIdentifier(node.callee.object, { name: 'console' }) &&
                t.isIdentifier(node.callee.property, { name: 'log' })) {

                node.arguments.forEach(arg => {
                    const val = this.resolveValue(arg);
                    this.addCode(`DISPLAY ${val}.`);
                });
            } else if (t.isIdentifier(node.callee)) {
                const funcName = node.callee.name;
                const sectionName = this.functions.get(funcName);
                if (sectionName) {
                    this.addCode(`PERFORM ${sectionName}.`);
                }
            } else if (t.isMemberExpression(node.callee) && t.isThisExpression(node.callee.object)) {
                // Handle this.method() calls
                const funcName = node.callee.property.name;
                const sectionName = this.functions.get(funcName);
                if (sectionName) {
                    this.addCode(`PERFORM ${sectionName}.`);
                }
            }
            path.skip();
        }

        if (t.isAssignmentExpression(node)) {
            if (t.isIdentifier(node.left)) {
                const targetVar = this.variables.get(node.left.name)?.name || this.addVariable(node.left.name, '9(9)');
                const val = this.resolveExpression(node.right, targetVar);
                if (val) this.addCode(`MOVE ${val} TO ${targetVar}.`);
            } else if (t.isMemberExpression(node.left)) {
                if (t.isThisExpression(node.left.object)) {
                    // this.prop = val
                    const propName = node.left.property.name;
                    const targetVar = this.variables.get(propName)?.name || this.addVariable(propName, '9(9)');
                    const val = this.resolveExpression(node.right, targetVar);
                    if (val) this.addCode(`MOVE ${val} TO ${targetVar}.`);
                } else {
                    // Array assignment
                    const arrName = node.left.object.name;
                    const index = this.resolveValue(node.left.property);
                    const arrVar = this.variables.get(arrName);
                    if (arrVar && arrVar.isArray) {
                        let finalIndex = index;
                        if (!isNaN(index)) {
                            finalIndex = parseInt(index) + 1;
                        } else {
                            this.addCode(`COMPUTE WS-TEMP-NUM = ${index} + 1.`);
                            finalIndex = 'WS-TEMP-NUM';
                        }
                        const val = this.resolveValue(node.right);
                        this.addCode(`MOVE ${val} TO ${arrVar.name} (${finalIndex}).`);
                    }
                }
            }
            path.skip();
        }

        if (t.isIfStatement(node)) {
            const condition = this.resolveCondition(node.test);
            this.addCode(`IF ${condition}`);
            path.get('consequent').traverse({ enter: (p) => this.visit(p) });
            if (node.alternate) {
                this.addCode('ELSE');
                path.get('alternate').traverse({ enter: (p) => this.visit(p) });
            }
            this.addCode('END-IF.');
            path.skip();
        }

        if (t.isWhileStatement(node)) {
            const whileCond = this.resolveCondition(node.test);
            this.addCode(`PERFORM UNTIL NOT (${whileCond})`);
            path.get('body').traverse({ enter: (p) => this.visit(p) });
            this.addCode('END-PERFORM.');
            path.skip();
        }
    }

    resolveExpression(node, targetVar) {
        if (t.isBinaryExpression(node)) {
            this.handleBinaryExpression(node, targetVar);
            return null;
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
        if (t.isStringLiteral(node)) return `"${node.value}"`;
        if (t.isNumericLiteral(node)) return node.value;
        if (t.isIdentifier(node)) return this.variables.get(node.name)?.name || node.name;
        if (t.isMemberExpression(node)) {
            if (t.isThisExpression(node.object)) {
                return this.variables.get(node.property.name)?.name || node.property.name;
            }
            const arrName = node.object.name;
            const index = this.resolveValue(node.property);
            const arrVar = this.variables.get(arrName);
            if (arrVar && arrVar.isArray) {
                if (!isNaN(index)) return `${arrVar.name} (${parseInt(index) + 1})`;
                return `${arrVar.name} (${index})`;
            }
        }
        return '0';
    }

    resolveCondition(node) {
        if (t.isBinaryExpression(node)) {
            const left = this.resolveValue(node.left);
            const right = this.resolveValue(node.right);
            let op = node.operator;
            if (op === '===') op = '=';
            if (op === '!==') op = 'NOT =';
            return `${left} ${op} ${right}`;
        }
        return '1 = 1';
    }
}

module.exports = Transpiler;
