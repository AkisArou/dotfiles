/** @jsxImportSource @opentui/react */
import { useEffect, useRef, useState } from "react";
import { extend } from "@opentui/react";
import { EmbeddedTerminalRenderable } from "@opentui/core";
import { connectTerminal } from "signalbox/client/terminal";
import type { TerminalClient, TerminalState } from "signalbox/client/terminal";
import { useTerminalEndpoint } from "@signalbox/kit/shared/terminal";
import { colors as c } from "./theme.ts";

extend({ "agent-terminal": EmbeddedTerminalRenderable });
declare module "@opentui/react" {
  interface OpenTUIComponents {
    "agent-terminal": typeof EmbeddedTerminalRenderable;
  }
}

export function NativeTerminal({
  id,
  interactive,
  controlRequest,
  onControl,
}: {
  id: string;
  interactive: boolean;
  controlRequest: number;
  onControl(owned: boolean): void;
}) {
  const url = useTerminalEndpoint();
  const terminal = useRef<EmbeddedTerminalRenderable>(null);
  const client = useRef<TerminalClient>(undefined);
  const pending = useRef(false),
    notify = useRef(onControl);
  notify.current = onControl;
  // The terminal's laid-out size. Connecting before layout would report a
  // placeholder size and resize the agent's window to it.
  const size = useRef<{ cols: number; rows: number }>(undefined);
  const [laidOut, setLaidOut] = useState(false);
  const [state, setState] = useState<TerminalState>({
    status: "connecting",
    owned: false,
    busy: false,
  });
  // A layout equal to the renderable's default size reports no resize; take the
  // measured size after the first frames instead, once it is a real size.
  useEffect(() => {
    const timer = setTimeout(() => {
      const view = terminal.current;
      if (size.current || !view || view.width < 2 || view.height < 2) return;
      size.current = { cols: Math.floor(view.width), rows: Math.floor(view.height) };
      setLaidOut(true);
    }, 50);
    return () => clearTimeout(timer);
  }, []);
  useEffect(() => {
    if (!url || !terminal.current || !laidOut || !size.current) return;
    const view = terminal.current;
    const connection = (client.current = connectTerminal({
      url,
      id,
      size: size.current,
      output(data) {
        view.write(data);
      },
      reset() {
        view.write("\x1bc");
      },
    }));
    setState(connection.getSnapshot());
    const unsubscribe = connection.subscribe(() => {
      const value = connection.getSnapshot();
      setState(value);
      notify.current(value.owned);
      if (value.owned) view.focus();
      else view.blur();
      if (value.status === "disconnected") pending.current = false;
      if (value.status === "ready" && pending.current) {
        pending.current = false;
        connection.claim();
      }
    });
    return () => {
      unsubscribe();
      connection.close();
      client.current = undefined;
      notify.current(false);
    };
  }, [id, url, laidOut]);
  useEffect(() => {
    pending.current = interactive && controlRequest > 0;
    if (!interactive) {
      terminal.current?.blur();
      notify.current(false);
      client.current?.release();
    } else if (client.current?.getSnapshot().status === "ready") {
      pending.current = false;
      client.current.claim();
    }
  }, [interactive, controlRequest]);
  return (
    <box flexDirection="column" flexGrow={1} minHeight={0} minWidth={0}>
      <text height={1} fg={state.owned ? c.accent : c.muted} wrapMode="none" truncate>
        {!url
          ? "No terminal endpoint configured"
          : (state.message ??
            (state.owned
              ? "INPUT · Ctrl+] returns to Signalbox; Ctrl+C goes to the program"
              : `${state.status} · ${state.busy ? "controlled in another viewer" : "Enter to type"}`))}
      </text>
      <agent-terminal
        ref={terminal}
        flexGrow={1}
        // flexGrow stretches only the column's height; without an explicit width
        // the renderable keeps its default 80 columns and reports that size.
        width="100%"
        minWidth={0}
        minHeight={0}
        transparentBackground
        maxScrollback={5000}
        selectable
        onTerminalResize={(cols, rows) => {
          if (cols < 2 || rows < 2) return;
          size.current = { cols, rows };
          if (client.current) client.current.resize({ cols, rows });
          else setLaidOut(true);
        }}
        onData={(bytes, source) => {
          const text = new TextDecoder().decode(bytes);
          if (source === "response") client.current?.response(text);
          else client.current?.input(text);
        }}
      />
    </box>
  );
}
