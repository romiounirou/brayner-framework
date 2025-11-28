<script setup>
import { ref } from 'vue';

const props = defineProps({
  title: String,
  status: String,
  isInput: Boolean,
  modelValue: String,
  placeholder: String,
  readOnly: Boolean
});

const emit = defineEmits(['update:modelValue', 'copy']);

const copyBtnText = ref('Copy');
const copyBtnColor = ref('');
const copyBtnBorder = ref('');

const handleCopy = () => {
  navigator.clipboard.writeText(props.modelValue).then(() => {
    copyBtnText.value = 'Copied!';
    copyBtnColor.value = '#4ade80';
    copyBtnBorder.value = '#4ade80';
    
    setTimeout(() => {
      copyBtnText.value = 'Copy';
      copyBtnColor.value = '';
      copyBtnBorder.value = '';
    }, 2000);
  });
};
</script>

<template>
  <div class="editor-pane">
    <div class="pane-header">
      <span class="lang-label">{{ title }}</span>
      
      <div v-if="!isInput" class="header-controls">
        <button 
          class="icon-btn" 
          title="Copy Code" 
          @click="handleCopy"
          :style="{ color: copyBtnColor, borderColor: copyBtnBorder }"
        >
          <svg v-if="copyBtnText === 'Copy'" width="16" height="16" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M8 4V16C8 17.1046 8.89543 18 10 18H18C19.1046 18 20 17.1046 20 16V7.24264C20 6.7122 19.7893 6.20357 19.4142 5.82843L16.1716 2.58579C15.7964 2.21071 15.2878 2 14.7574 2H10C8.89543 2 8 2.89543 8 4Z" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M16 18V20C16 21.1046 15.1046 22 14 22H6C4.89543 22 4 21.1046 4 20V8C4 6.89543 4.89543 6 6 6H8" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
          </svg>
          <svg v-else width="16" height="16" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
             <path d="M20 6L9 17L4 12" stroke="#4ade80" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
          </svg>
          {{ copyBtnText }}
        </button>
        <span class="status">{{ status }}</span>
      </div>
      <span v-else class="status">{{ status }}</span>
    </div>
    
    <textarea 
      v-if="isInput"
      :value="modelValue"
      @input="$emit('update:modelValue', $event.target.value)"
      :placeholder="placeholder"
      spellcheck="false"
    ></textarea>
    
    <pre v-else><code class="language-cobol">{{ modelValue }}</code></pre>
  </div>
</template>
