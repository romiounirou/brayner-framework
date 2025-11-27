# Brayner Framework 🚀

A modern JavaScript to COBOL transpiler with real-time web interface.

## Features

- ✅ **Control Flow**: `if/else`, `while`, `for` loops
- ✅ **Functions**: Function declarations and calls
- ✅ **Arrays**: Fixed-size arrays with COBOL `OCCURS` tables
- ✅ **Operators**: Arithmetic and comparison operators
- ✅ **Web Interface**: Real-time transpilation preview
- ✅ **CLI Tool**: Command-line interface for batch processing

## Installation

```bash
npm install
```

## Usage

### Web Interface

1. Start the server:
```bash
node server.js
```

2. Open `http://localhost:3000` in your browser
3. Type JavaScript on the left, see COBOL on the right in real-time!

### CLI

```bash
node index.js <input_file.js>
```

The transpiler will generate a `.cbl` file and attempt to compile/run it if GnuCOBOL is installed.

## Example

**JavaScript:**
```javascript
var x = 10;
var y = 20;
var sum = x + y;
console.log(sum);
```

**Generated COBOL:**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRAYNER-APP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  X PIC 9(9).
       77  Y PIC 9(9).
       77  SUM PIC 9(9).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 10 TO X.
           MOVE 20 TO Y.
           COMPUTE SUM = X + Y.
           DISPLAY SUM.
           STOP RUN.
```

## Supported JavaScript Features

- Variable declarations (`var`, `let`, `const`)
- Arithmetic operations (`+`, `-`, `*`, `/`)
- Comparison operators (`<`, `>`, `=`, `===`, `!==`)
- Control flow (`if/else`, `while`, `for`)
- Arrays (fixed-size with auto-indexing conversion)
- Functions (basic declarations and calls)
- `console.log()` → `DISPLAY`

## Limitations

- Objects not supported (COBOL has no object model)
- Async/await and promises not supported
- Dynamic typing limited to string/number inference
- DOM manipulation not applicable

## License

MIT
