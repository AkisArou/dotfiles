// The app's height in --app-height: the window's inner height, which is what
// iOS draws in a home-screen app (measured on the phone); 100dvh and a fixed
// inset can disagree with it there.

export function appHeight(): number {
  return window.innerHeight;
}

export function followAppHeight(): void {
  const apply = () =>
    document.documentElement.style.setProperty("--app-height", `${appHeight()}px`);
  apply();
  window.addEventListener("resize", apply);
  window.addEventListener("orientationchange", () => setTimeout(apply, 250));
}
