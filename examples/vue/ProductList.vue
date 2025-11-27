<template>
  <div class="product-list">
    <div v-for="p in products">{{ p.name }}</div>
  </div>
</template>

<script>
export default {
  data() {
    return {
      products: []
    }
  },
  methods: {
    addProduct() {
      var newProduct = "Mouse";
      console.log(newProduct);
    }
  }
}
</script>
