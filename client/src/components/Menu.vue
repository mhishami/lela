<template>
  <nav class="uk-navbar uk-margin-large-bottom">
    <a class="uk-navbar-brand uk-hidden-small" href=""><img class="logo" src="../assets/logo.png"></a>
    <ul class="uk-navbar-nav uk-hidden-small">
      <li class="uk-active">
        <a href="/">Home</a>
      </li>
      <li>
        <a href="">Assets</a>
      </li>
      <li>
        <a href="">Dashboard</a>
      </li>
    </ul>
    <div class="uk-navbar-content uk-navbar-flip">
      <div class="uk-button-group">
        <button class="uk-button uk-button-success" data-uk-modal="{target: '#register'}">Register</button>
        <button class="uk-button uk-button-primary">Login</button>
      </div>

      <!-- This is the modal -->
      <div id="register" class="uk-modal">
        <div class="uk-modal-dialog uk-modal-dialog-lightbox">
          <a href="" class="uk-modal-close uk-close uk-close-alt"></a>

          <div class="uk-grid">
            <div class="uk-width-1-1">
              <form class="uk-form uk-form-stacked">
                <div class="uk-form-row">
                  <input placeholder="Username"></input>
                </div>
                <div class="uk-form-row">
                  <input placeholder="Password"></input>
                </div>
              </form>
            </div>
          </div>
        </div>
      </div>

    </div>

    <!-- right menu -->
    <a href="#offcanvas" class="uk-navbar-toggle uk-visible-small" data-uk-offcanvas></a>
    <div class="uk-navbar-brand uk-navbar-center uk-visible-small"><img class="logo" src="../assets/logo.png"></div>
  </nav>

  <div id="offcanvas" class="uk-offcanvas">
    <div class="uk-offcanvas-bar">
      <ul class="uk-nav uk-nav-offcanvas">
        <li class="uk-active">
          <a href="/">Home</a>
        </li>
        <li>
          <a href="">Assets</a>
        </li>
        <li>
          <a href="">Dashboard</a>
        </li>
      </ul>
    </div>
  </div>

</template>

<style type="text/scss">
  .logo {
    max-width: 30px;
    max-height: 30px;
  }

</style>