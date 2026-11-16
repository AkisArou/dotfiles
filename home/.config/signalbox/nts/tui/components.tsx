/** @jsxImportSource @opentui/react */
import { useEffect, useRef } from "react";
import type { ReactNode, RefObject } from "react";
import { TextAttributes } from "@opentui/core";
import type { ColorInput, ScrollBoxRenderable } from "@opentui/core";
import { colors as c } from "./theme.ts";
import { useSelection } from "./selection.tsx";

export function Heading({ children }: { children: ReactNode }) {
  return (
    <text fg={c.accent} attributes={TextAttributes.BOLD} flexShrink={0}>
      {children}
    </text>
  );
}

export function Section({ title, children }: { title: string; children: ReactNode }) {
  return (
    <box flexDirection="column" marginBottom={1} flexShrink={0}>
      <Heading>{title}</Heading>
      <box flexDirection="column" paddingLeft={2}>
        {children}
      </box>
    </box>
  );
}

export function Line({ children, color = c.text }: { children: ReactNode; color?: ColorInput }) {
  return (
    <text fg={color} flexShrink={0}>
      {children}
    </text>
  );
}

export function ListRow({
  id,
  label,
  end,
  selected,
  focused,
  color = c.text,
  onSelect,
}: {
  id: string;
  label: string;
  end?: string;
  selected: boolean;
  focused: boolean;
  color?: ColorInput;
  onSelect(): void;
}) {
  const selection = useSelection();
  const highlighted = selected && focused;
  const attributes = selected ? TextAttributes.BOLD | (highlighted ? selection.attributes : 0) : 0;
  return (
    <box id={id} height={1} flexShrink={0} flexDirection="row" onMouseDown={onSelect}>
      <text
        fg={highlighted ? selection.text : color}
        bg={highlighted ? selection.background : c.background}
        attributes={attributes}
        flexGrow={1}
        flexShrink={1}
        minWidth={0}
        wrapMode="none"
        truncate
      >{` ${label}`}</text>
      {end && (
        <text
          fg={highlighted ? selection.text : color}
          bg={highlighted ? selection.background : c.background}
          attributes={attributes}
          width={Math.min(22, end.length + 2)}
          wrapMode="none"
          truncate
          textAlign="right"
        >{`${end} `}</text>
      )}
    </box>
  );
}

export function ScrollPane({
  children,
  focused,
  selected,
  scrollRef,
  onFocus,
}: {
  children: ReactNode;
  focused: boolean;
  selected?: string;
  scrollRef?: RefObject<ScrollBoxRenderable | null>;
  onFocus?(): void;
}) {
  const own = useRef<ScrollBoxRenderable>(null),
    ref = scrollRef ?? own;
  useEffect(() => {
    if (selected) ref.current?.scrollChildIntoView(selected, { align: "nearest" });
  }, [selected, ref]);
  return (
    <scrollbox
      ref={ref}
      flexGrow={1}
      minWidth={0}
      minHeight={0}
      focused={focused}
      scrollY
      onMouseDown={onFocus}
      contentOptions={{ paddingRight: 1 }}
      scrollbarOptions={{
        trackOptions: { backgroundColor: c.background, foregroundColor: c.muted },
        arrowOptions: { backgroundColor: c.background, foregroundColor: c.muted },
      }}
    >
      {children}
    </scrollbox>
  );
}

export function ListDetail({
  width,
  focus,
  listWidth,
  list,
  detail,
}: {
  width: number;
  focus: "list" | "detail";
  listWidth: number;
  list: ReactNode;
  detail: ReactNode;
}) {
  if (width < 100)
    return (
      <box flexGrow={1} minHeight={0}>
        {focus === "list" ? list : detail}
      </box>
    );
  return (
    <box flexDirection="row" flexGrow={1} minHeight={0}>
      <box width={listWidth} minHeight={0}>
        {list}
      </box>
      <box width={1} border={["left"]} borderColor={c.border} marginLeft={1} marginRight={1} />
      <box flexGrow={1} minWidth={0} minHeight={0}>
        {detail}
      </box>
    </box>
  );
}
