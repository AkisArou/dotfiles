export default function layout(ctx) {
  return {
    type: "workspace",
    props: { id: "root" },
    children: [
      {
        type: "group",
        props: { id: "frame" },
        children: [
          {
            type: "slot",
            props: { id: "master", take: 1 },
            children: [],
          },
          ctx.windows.length > 1
            ? {
                type: "group",
                props: { id: "stack" },
                children: [
                  {
                    type: "slot",
                    props: { class: "stack-item" },
                    children: [],
                  },
                ],
              }
            : null,
        ].filter(Boolean),
      },
    ],
  };
}
