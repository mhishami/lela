<template>
  <div class="uk-container uk-container-center uk-margin-top uk-margin-large-bottom">
    <div id="app">
      <menu></menu>
      <hello></hello>
    </div>
  </div>
</template>

<script>
import Hello from './components/Hello'
import Menu from './components/Menu'

export default {
  components: {
    Hello,
    Menu
  }
}
</script>

