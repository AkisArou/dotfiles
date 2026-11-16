import { useEffect, useRef, useState } from "react";
import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { Button } from "@astryxdesign/core/Button";
import { connectTerminal } from "signalbox/client/terminal";
import type { TerminalClient, TerminalState } from "signalbox/client/terminal";
import { useTerminalEndpoint } from "../shared/terminal.ts";
import "@xterm/xterm/css/xterm.css";

export function NativeTerminal({ id }: { id: string }) {
  const url = useTerminalEndpoint();
  const element = useRef<HTMLDivElement>(null),
    client = useRef<TerminalClient>(undefined);
  const [state, setState] = useState<TerminalState>({
    status: "connecting",
    owned: false,
    busy: false,
  });
  useEffect(() => {
    if (!url || !element.current) return;
    const term = new Terminal({
      fontSize: 13,
      fontFamily: "monospace",
      scrollback: 5000,
      disableStdin: true,
      screenReaderMode: true,
    });
    const fit = new FitAddon();
    term.loadAddon(fit);
    term.open(element.current);
    fit.fit();
    const applyTheme = () => {
      const style = getComputedStyle(element.current!);
      term.options.theme = { background: style.backgroundColor, foreground: style.color };
    };
    applyTheme();
    const theme = element.current.closest("[data-theme]");
    const themeChanges = new MutationObserver(applyTheme);
    if (theme)
      themeChanges.observe(theme, {
        attributes: true,
        attributeFilter: ["data-theme", "class", "style"],
      });
    const connection = (client.current = connectTerminal({
      url,
      id,
      size: { cols: term.cols, rows: term.rows },
      output: (data) => term.write(data),
      reset: () => term.reset(),
    }));
    setState(connection.getSnapshot());
    const unsubscribe = connection.subscribe(() => {
      const value = connection.getSnapshot();
      setState(value);
      term.options.disableStdin = !value.owned;
      if (value.owned) term.focus();
    });
    const input = term.onData((data) => {
      if (connection.getSnapshot().owned) connection.input(data);
      // Read-only viewers do not send either input or terminal responses.
    });
    term.attachCustomKeyEventHandler((event) => {
      if (event.type === "keydown" && event.ctrlKey && event.key === "]") {
        connection.release();
        event.preventDefault();
        return false;
      }
      return connection.getSnapshot().owned;
    });
    const observer = new ResizeObserver(() => {
      fit.fit();
      connection.resize({ cols: term.cols, rows: term.rows });
    });
    observer.observe(element.current);
    return () => {
      observer.disconnect();
      themeChanges.disconnect();
      unsubscribe();
      input.dispose();
      connection.close();
      term.dispose();
      client.current = undefined;
    };
  }, [id, url]);
  return (
    <div className="native-terminal">
      <div className="terminal-toolbar">
        <span role="status">
          {!url
            ? "No terminal endpoint"
            : (state.message ??
              (state.owned
                ? "Input active · Ctrl+] releases control"
                : `${state.status} · ${state.busy ? "controlled in another viewer" : "read-only"}`))}
        </span>
        <Button
          label={state.owned ? "Release input" : "Take input"}
          variant="secondary"
          isDisabled={state.status !== "ready" || (!state.owned && state.busy)}
          onClick={() => (state.owned ? client.current?.release() : client.current?.claim())}
        />
      </div>
      <div className="terminal-screen" ref={element} aria-label="Native terminal" />
    </div>
  );
}
