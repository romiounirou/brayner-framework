<script setup>
import { ref, watch } from 'vue';
import Header from './components/Header.vue';
import EditorPane from './components/EditorPane.vue';
import CreatorPopup from './components/CreatorPopup.vue';

const isReverse = ref(false);
const inputCode = ref('');
const outputCode = ref('');
const isPopupOpen = ref(false);
let timeout = null;

const jsPlaceholder = `// Write your JavaScript here...
var msg = 'Hello World';
console.log(msg);`;

const cobolPlaceholder = `// Write your COBOL here...
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.`;

const transpile = async (code) => {
  if (!code.trim()) {
    outputCode.value = '';
    return;
  }

  try {
    const endpoint = isReverse.value ? '/api/reverse' : '/api/transpile';
    const response = await fetch(endpoint, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ code })
    });

    const data = await response.json();

    if (data.error) {
      outputCode.value = `Error: ${data.error}`;
    } else {
      outputCode.value = isReverse.value ? data.javascript : data.cobol;
    }
  } catch (err) {
    outputCode.value = `Network Error: ${err.message}`;
  }
};

watch(inputCode, (newVal) => {
  clearTimeout(timeout);
  timeout = setTimeout(() => {
    transpile(newVal);
  }, 500);
});

const toggleMode = () => {
  isReverse.value = !isReverse.value;
  inputCode.value = '';
  outputCode.value = '';
};
</script>

<template>
  <Header 
    :isReverse="isReverse" 
    @toggle-mode="toggleMode" 
    @open-popup="isPopupOpen = true"
  />

  <main class="container">
    <EditorPane 
      :title="isReverse ? 'COBOL' : 'JavaScript'" 
      status="Input" 
      :isInput="true"
      v-model="inputCode"
      :placeholder="isReverse ? cobolPlaceholder : jsPlaceholder"
    />

    <EditorPane 
      :title="isReverse ? 'JavaScript' : 'COBOL'" 
      status="Real-time Preview" 
      :isInput="false"
      :modelValue="outputCode"
    />
  </main>

  <CreatorPopup 
    :isOpen="isPopupOpen" 
    @close="isPopupOpen = false"
  />
</template>
