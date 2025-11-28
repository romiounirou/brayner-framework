const Transpiler = require('./transpiler');

const vueCode = `
<template>
  <div>Hello</div>
</template>

<script>
export default {
  data() {
    return {
      message: "Hello COBOL",
      count: 10
    }
  },
  methods: {
    increment() {
      this.count++;
    }
  },
  mounted() {
    console.log(this.message);
    this.increment();
  }
}
</script>
`;

try {
    const transpiler = new Transpiler();
    const cobol = transpiler.transpile(vueCode);
    const fs = require('fs');
    fs.writeFileSync('output.cbl', cobol);
    console.log('Transpilation complete. Check output.cbl');
} catch (err) {
    console.error(err);
}
