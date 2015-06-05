// term_.prefs_.set('background-color', 'black')
// term_.prefs_.set('foreground-color', 'gray')
// term_.prefs_.set('cursor-color', 'rgba(100, 100, 10, 0.5)')

// Solarized Dark
term_.prefs_.set('background-color', "#002b36");
term_.prefs_.set('foreground-color', "#eee8d5");
term_.prefs_.set('cursor-color', "#eee8d5");
term_.prefs_.set('color-palette-overrides', [
    "#073642",
    "#859900",
    "#b58900",
    "#268bd2",
    "#d33682",
    "#2aa198",
    "#eee8d5",
    "#002b36",
    "#cb4b16",
    "#586e75",
    "#657b83",
    "#839496",
    "#6c71c4",
    "#93a1a1",
    "#fdf6e3",
]);

// // Solarized Light
// term_.prefs_.set('background-color', "#eee8d5");
// term_.prefs_.set('foreground-color', "#002b36");
// term_.prefs_.set('cursor-color', "#002b36");
// term_.prefs_.set('color-palette-overrides', [
//     "#073642",
//     "#dc322f",
//     "#859900",
//     "#b58900",
//     "#268bd2",
//     "#d33682",
//     "#2aa198",
//     "#eee8d5",
//     "#002b36",
//     "#cb4b16",
//     "#586e75",
//     "#657b83",
//     "#839496",
//     "#6c71c4",
//     "#93a1a1",
//     "#fdf6e3",
// ]);

// // Monokai
// term_.prefs_.set('background-color', "#101010");
// term_.prefs_.set('foreground-color', "#d0d0d0");
// term_.prefs_.set('cursor-color', "#d0d0d0");
// term_.prefs_.set('color-palette-overrides', [
//   '#101010',
//   '#960050',
//   '#66aa11',
//   '#c47f2c',
//   '#30309b',
//   '#7e40a5',
//   '#3579a8',
//   '#9999aa',
//   '#303030',
//   '#ff0090',
//   '#80ff00',
//   '#ffba68',
//   '#5f5fee',
//   '#bb88dd',
//   '#4eb4fa',
//   '#d0d0d0'
// ]);

term_.prefs_.set('audible-bell-sound', '')
term_.prefs_.set('enable-bold-as-bright', true);

term_.prefs_.set('font-size', 17)
term_.prefs_.set('font-smoothing', 'subpixel-antialiased')
term_.prefs_.set('font-smoothing', 'antialiased')
// term_.prefs_.set('user-css', 'http://fonts.googleapis.com/css?family=Anonymous+Pro|Source+Code+Pro|Ubuntu+Mono|Droid+Sans+Mono')
term_.prefs_.set('user-css-text', `
@font-face {
  font-family: "Anonymous Pro";
  src: url(https://cdn.rawgit.com/wernight/powerline-web-fonts/8040cf32c146c7cd4f776c1484d23dc40685c1bc/fonts/AnonymousPro.woff2) format("woff2");
}`)
term_.prefs_.set('font-family', 'Anonymous Pro');
term_.prefs_.set('environment', {TERM: 'xterm-256color'})
term_.prefs_.set('alt-is-meta', true)
term_.prefs_.set('keybindings', {
   "Ctrl+Shift+Space": "PASS",
})
term_.prefs_.set('user-css-text', `
@font-face {
  font-family: "Anonymous Pro";
  src: local('Anonymous Pro'), url(https://cdn.rawgit.com/wernight/powerline-web-fonts/8040cf32c146c7cd4f776c1484d23dc40685c1bc/fonts/AnonymousPro.woff2) format("woff2");
  font-style: normal;
  font-weight: normal;
}
body {
  font-family: 'Anonymous Pro' !important;
}`)
term_.prefs_.set('user-css-text', `
@import url('https://fonts.googleapis.com/css?family=PT+Mono');
@import url('https://fonts.googleapis.com/css?family=Oxygen+Mono');
@import url('https://cdn.jsdelivr.net/npm/hack-font@3/build/web/hack.css');

x-screen {
  font-family: 'Fantasque Sans Mono', 'PT Mono', 'Oxygen Mono', monospace !important;
  font-size: 13px; 
  background-color: #282a36 !important;                       
  color: #f8f8f2 !important; 
  -webkit-font-smoothing: antialiased !important;
}`)
