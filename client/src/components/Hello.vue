<template>

  <div class="uk-grid" data-uk-grid-margin>
    <div class="uk-width-medium-1-1">
      <div class="uk-vertical-align uk-text-center">
        <div class="uk-vertical-align-middle uk-width-1-2">
          <h1 class="uk-heading-large">{{ msg }}</h1>
        </div>
      </div>
    </div>
  </div>

  <div class="uk-grid" data-uk-grid-margin>
    <div class="uk-width-medium-1-1 uk-text-center" data-uk-margin>
      <p>
        Welcome to your Vue.js app. To get started, take a look at the
        <a href="https://github.com/vuejs-templates/webpack#folder-structure" target="_blank">README</a>
        of this template.<br/>
        If you have any issues with the setup, please file an issue at this template's repository.
      </p>
      <p>
        For advanced configurations, checkout the docs for
        <a href="http://webpack.github.io/" target="_blank">Webpack</a> and
        <a href="http://vuejs.github.io/vue-loader/" target="_blank">vue-loader</a>.
      </p>
      <p>
        You may also want to checkout
        <a href="https://github.com/vuejs/vue-router/" target="_blank">vue-router</a> for routing and
        <a href="https://github.com/vuejs/vuex/" target="_blank">vuex</a> for state management.
      </p>
    </div>

  </div>
</template>

<script>
export default {
  data () {
    return {
      // note: changing this line won't causes changes
      // with hot-reload because the reloaded component
      // preserves its current state and we are modifying
      // its initial state.
      msg: 'Hello World!'
    }
  }
}
</script>
