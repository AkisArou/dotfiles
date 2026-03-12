import "./index.css"

export default function layout() {
  return (
    <workspace id="root">
      <slot id="main" />
    </workspace>
  );
}
