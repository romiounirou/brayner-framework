<script setup>
defineProps({
  isReverse: Boolean
});

const emit = defineEmits(['toggle-mode', 'open-popup']);
</script>

<template>
  <header>
    <div class="logo-container" id="logo-container" @click="emit('open-popup')">
      <svg class="logo-icon" width="40" height="40" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M14.5 9.5L19 5M5 19L9.5 14.5M5 19L3 21M5 19L2 16M19 5L21 3M19 5L22 8M9.5 14.5C10.5 13.5 10.5 12 9.5 11C8.5 10 7 10 6 11L3 14L10 21L13 18C14 17 14 15.5 13 14.5C12 13.5 10.5 13.5 9.5 14.5Z" stroke="#38bdf8" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        <path d="M14.5 9.5C13.5 10.5 12 10.5 11 9.5C10 8.5 10 7 11 6L14 3L21 10L18 13C17 14 15.5 14 14.5 13C13.5 12 13.5 10.5 14.5 9.5Z" stroke="#94a3b8" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
      </svg>
      <h1 class="logo-text">Brayner <span class="highlight">Framework</span></h1>
    </div>
    <p class="subtitle">Quer solidez? vem de alicate</p>
    <p class="mode-info">Bidirectional Transpiler: <span id="mode-display">{{ isReverse ? 'COBOL → JavaScript' : 'JavaScript → COBOL' }}</span></p>
    <button id="swap-btn" class="swap-button" @click="emit('toggle-mode')">⇄ Swap Direction</button>
  </header>
</template>
