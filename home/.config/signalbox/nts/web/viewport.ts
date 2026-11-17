// The app's height in --app-height. An iOS home-screen app under a translucent
// status bar measures its viewport short by the status bar's height (100dvh and a
// fixed inset alike), leaving a band at the bottom; there the screen's own height
// is the app's. Elsewhere the window's inner height is.

const standaloneIos = () => (navigator as Navigator & { standalone?: boolean }).standalone === true;

export function appHeight(): number {
  if (!standaloneIos()) return window.innerHeight;
  const landscape = window.matchMedia("(orientation: landscape)").matches;
  const { width, height } = window.screen;
  return landscape ? Math.min(width, height) : Math.max(width, height);
}

export function followAppHeight(): void {
  const apply = () =>
    document.documentElement.style.setProperty("--app-height", `${appHeight()}px`);
  apply();
  window.addEventListener("resize", apply);
  window.addEventListener("orientationchange", () => setTimeout(apply, 250));
}
