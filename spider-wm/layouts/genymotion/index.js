export default function layout() {
  return {
    type: "workspace",
    props: { id: "root" },
    children: [
      {
        type: "slot",
        props: { id: "main" },
        children: [],
      },
    ],
  };
}
