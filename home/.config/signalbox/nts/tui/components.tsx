/** @jsxImportSource @opentui/react */
import { useEffect, useRef, useState } from "react";
import type { ReactNode, RefObject } from "react";
import { TextAttributes } from "@opentui/core";
import type { ColorInput, ScrollBoxRenderable } from "@opentui/core";
import { colors as c } from "./theme.ts";
import { useSelection } from "./selection.tsx";

export function Heading({ children }: { children: ReactNode }) {
  return (
    <text fg={c.text} attributes={TextAttributes.BOLD} flexShrink={0}>
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

const LABEL_MIN = 32;
const META_MAX = 26;
/** Cut at the end: the start of a name or a path says the most. */
const clip = (text: string, max: number) =>
  text.length > max ? `${text.slice(0, max - 1)}…` : text;

export function ListRow({
  id,
  label,
  meta,
  details = [],
  end,
  selected,
  focused,
  color = c.text,
  onSelect,
}: {
  id: string;
  label: string;
  /** Muted metadata after the label (a task's group): it gives way to the label. */
  meta?: string;
  /** Muted lines under the label, for an item whose metadata needs room (an agent's issue and tree). */
  details?: readonly string[];
  end?: string;
  selected: boolean;
  focused: boolean;
  color?: ColorInput;
  onSelect(): void;
}) {
  const selection = useSelection();
  const highlighted = selected && focused;
  const attributes = selected ? TextAttributes.BOLD | (highlighted ? selection.attributes : 0) : 0;
  // The row measures itself: the label keeps LABEL_MIN columns (or all of a short one), and the
  // metadata takes what is left up to META_MAX, cut at its end, or stays out when too little is.
  const [width, setWidth] = useState(0);
  const endWidth = end ? Math.min(22, end.length + 2) : 0;
  const room = width - endWidth - 1 - Math.min(label.length, LABEL_MIN);
  const metaWidth = meta && room >= 10 ? Math.min(META_MAX, meta.length, room - 2) : 0;
  const detailColor = highlighted ? selection.text : selected ? c.text : c.muted;
  return (
    <box
      id={id}
      flexShrink={0}
      flexDirection="column"
      onMouseDown={onSelect}
      onSizeChange={function () {
        setWidth(this.width);
      }}
    >
      <box height={1} flexShrink={0} flexDirection="row">
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
        {meta && metaWidth > 0 && (
          <text
            fg={highlighted ? selection.text : c.muted}
            bg={highlighted ? selection.background : c.background}
            attributes={highlighted ? selection.attributes : 0}
            width={metaWidth + 2}
            flexShrink={0}
            wrapMode="none"
          >{`  ${clip(meta, metaWidth)}`}</text>
        )}
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
      {details.map((line, index) => (
        <text
          key={index}
          height={1}
          fg={detailColor}
          bg={highlighted ? selection.background : c.background}
          attributes={highlighted ? selection.attributes : 0}
          wrapMode="none"
          truncate
        >{`   ${line}`}</text>
      ))}
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
