<template>
  <div>{{ message }}</div>
</template>

<script>
export default {
  data() {
    return {
      message: 'Hello Vue'
    }
  },
  methods: {
    greet() {
      var name = "Vue User";
      console.log(name);
    }
  }
}
</script>
