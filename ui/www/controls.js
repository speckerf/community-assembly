$(function () {
  Shiny.addCustomMessageHandler('playback', function (state) {
    ['step', 'go', 'go_skip'].forEach(function (id) {
      document.getElementById(id).disabled = state.running || !state.initialized;
    });
    document.getElementById('stop').disabled = !state.running;
    const download = document.getElementById('download');
    download.classList.toggle('disabled', !state.initialized);
    download.setAttribute('aria-disabled', String(!state.initialized));
  });
});
