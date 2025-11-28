<script setup>
defineProps({
  isOpen: Boolean
});

const emit = defineEmits(['close']);
</script>

<template>
  <div class="popup-overlay" :class="{ active: isOpen }" @click.self="emit('close')">
    <div class="popup-content">
      <img src="/creator.png" alt="Creator" class="creator-photo">
      <h2>Criador</h2>
      <p><strong>Renan Brayner</strong></p>
      <p>Desenvolvedor do Brayner Framework</p>
    </div>
  </div>
</template>
