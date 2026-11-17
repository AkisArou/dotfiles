// An agent over the whole screen: its transcript to scroll (its pane's history,
// in colour, following the newest output), or its live terminal; and below, a
// composer that sends your words to it, with the keys its prompts ask for.
// Sending goes straight to its pane (nts.terminal-send): no viewer takes
// control, and the agent's window keeps its size.
import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { CSSProperties } from "react";
import { useClient } from "signalbox/client/react";
import { Icon } from "@astryxdesign/core/Icon";
import { IconButton } from "@astryxdesign/core/IconButton";
import { SegmentedControl, SegmentedControlItem } from "@astryxdesign/core/SegmentedControl";
import { Text } from "@astryxdesign/core/Text";
import { fitLine, paletteColour, parseAnsi } from "../shared/ansi.ts";
import type { Colour, Run } from "../shared/ansi.ts";
import { agentStates } from "../shared/glossary.ts";
import * as refs from "../shared/model.ts";
import { agentTone } from "../shared/model.ts";
import type { Agent } from "../shared/model.ts";
import { Status } from "./components.tsx";
import { NativeTerminal } from "./terminal.tsx";

/** Lines of history read for the transcript, and how often it is read again. */
const HISTORY_LINES = 3000;
const REFRESH_MS = 2000;

/** The keys an agent's prompts ask for: a choice, a confirmation, an interruption. */
const KEYS: readonly { label: string; key: string; title: string }[] = [
  { label: "Esc", key: "Escape", title: "Escape: interrupt or dismiss" },
  { label: "↑", key: "Up", title: "Up" },
  { label: "↓", key: "Down", title: "Down" },
  { label: "Enter", key: "Enter", title: "Enter: confirm" },
  { label: "1", key: "1", title: "Choose 1" },
  { label: "2", key: "2", title: "Choose 2" },
  { label: "3", key: "3", title: "Choose 3" },
  { label: "Tab", key: "Tab", title: "Tab" },
  { label: "⇧Tab", key: "BTab", title: "Shift+Tab: switch mode" },
  { label: "^C", key: "C-c", title: "Ctrl+C" },
];

/** Follows the visual viewport, so the composer rides above an on-screen keyboard. */
function useVisualViewport(): CSSProperties {
  const read = (): CSSProperties => {
    const view = window.visualViewport;
    return view ? { height: view.height, top: view.offsetTop } : { inset: 0 };
  };
  const [style, setStyle] = useState<CSSProperties>(read);
  useEffect(() => {
    const view = window.visualViewport;
    if (!view) return;
    const update = () => setStyle(read());
    view.addEventListener("resize", update);
    view.addEventListener("scroll", update);
    return () => {
      view.removeEventListener("resize", update);
      view.removeEventListener("scroll", update);
    };
  }, []);
  return style;
}

const colour = (value: Colour | undefined) =>
  !value
    ? undefined
    : "rgb" in value
      ? value.rgb
      : value.index < 16
        ? `var(--ansi-${value.index})`
        : paletteColour(value.index);

function runStyle(run: Run): CSSProperties | undefined {
  const { style } = run;
  if (!Object.keys(style).length) return undefined;
  const [fg, bg] = style.inverse
    ? [
        colour(style.bg) ?? "var(--transcript-background)",
        colour(style.fg) ?? "var(--transcript-text)",
      ]
    : [colour(style.fg), colour(style.bg)];
  return {
    ...(fg ? { color: fg } : {}),
    ...(bg ? { backgroundColor: bg } : {}),
    ...(style.bold ? { fontWeight: 600 } : {}),
    ...(style.dim ? { opacity: 0.62 } : {}),
    ...(style.italic ? { fontStyle: "italic" } : {}),
    ...(style.underline || style.strike
      ? {
          textDecoration: [style.underline && "underline", style.strike && "line-through"]
            .filter(Boolean)
            .join(" "),
        }
      : {}),
  };
}

/** The agent's pane as a transcript: read again every few seconds, following the end. */
function Transcript({ terminal, nudge }: { terminal: string; nudge: number }) {
  const client = useClient();
  const [text, setText] = useState<string>();
  const [error, setError] = useState<string>();
  const scroller = useRef<HTMLDivElement>(null);
  const following = useRef(true);
  const [behind, setBehind] = useState(false);
  const read = useCallback(
    async (signal?: AbortSignal) => {
      try {
        const result = await client.call(
          refs.terminalHistory,
          { terminal, lines: HISTORY_LINES },
          signal ? { signal } : {},
        );
        setText((old) => (old === result.text ? old : result.text));
        setError(undefined);
      } catch (failure) {
        if (!signal?.aborted)
          setError(failure instanceof Error ? failure.message : String(failure));
      }
    },
    [client, terminal],
  );
  useEffect(() => {
    const abort = new AbortController();
    void read(abort.signal);
    const timer = setInterval(() => {
      if (document.visibilityState === "visible") void read(abort.signal);
    }, REFRESH_MS);
    return () => {
      abort.abort();
      clearInterval(timer);
    };
  }, [read]);
  // After you send, read at once and go to the end.
  useEffect(() => {
    if (!nudge) return;
    following.current = true;
    const timer = setTimeout(() => void read(), 250);
    return () => clearTimeout(timer);
  }, [nudge, read]);
  // Trailing blank lines are the pane's empty screen, not output.
  const lines = useMemo(() => {
    const parsed = text === undefined ? [] : parseAnsi(text);
    while (parsed.length && parsed.at(-1)!.every((run) => !run.text.trim())) parsed.pop();
    // Lines fit the phone's width: padding goes, a padded background becomes a band.
    return parsed.map(fitLine);
  }, [text]);
  useLayoutEffect(() => {
    const element = scroller.current;
    if (element && following.current) element.scrollTop = element.scrollHeight;
  }, [lines]);
  const onScroll = () => {
    const element = scroller.current;
    if (!element) return;
    const atEnd = element.scrollHeight - element.scrollTop - element.clientHeight < 48;
    following.current = atEnd;
    setBehind(!atEnd);
  };
  const toEnd = () => {
    const element = scroller.current;
    if (!element) return;
    following.current = true;
    element.scrollTo({ top: element.scrollHeight, behavior: "smooth" });
  };
  return (
    <div className="transcript-wrap">
      <div className="transcript" ref={scroller} onScroll={onScroll}>
        {text === undefined && !error && (
          <Text type="supporting" color="secondary">
            Reading the agent's history…
          </Text>
        )}
        {error && (
          <Text type="supporting" color="secondary">
            {error}
          </Text>
        )}
        <pre className="transcript-text">
          {lines.map(({ runs, background }, index) => (
            <div
              key={index}
              className={background ? "transcript-line band" : "transcript-line"}
              style={background ? { backgroundColor: colour(background) } : undefined}
            >
              {runs.length
                ? runs.map((run, at) => (
                    <span key={at} style={runStyle(run)}>
                      {run.text}
                    </span>
                  ))
                : " "}
            </div>
          ))}
        </pre>
      </div>
      {behind && (
        <button type="button" className="transcript-latest" onClick={toEnd}>
          Latest ↓
        </button>
      )}
    </div>
  );
}

/** Your words to the agent: a growing box and Send, with the keys its prompts need. */
function Composer({ terminal, sent }: { terminal: string; sent(): void }) {
  const client = useClient();
  const [text, setText] = useState("");
  const [sending, setSending] = useState(false);
  const [error, setError] = useState<string>();
  const box = useRef<HTMLTextAreaElement>(null);
  // The box grows with what you write, up to about six lines, then scrolls.
  useLayoutEffect(() => {
    const element = box.current;
    if (!element) return;
    element.style.height = "auto";
    element.style.height = `${Math.min(element.scrollHeight, 150)}px`;
  }, [text]);
  const send = async (input: { text?: string; keys?: string[] }) => {
    setSending(true);
    setError(undefined);
    try {
      await client.call(refs.terminalSend, { terminal, ...input });
      sent();
      return true;
    } catch (failure) {
      setError(failure instanceof Error ? failure.message : String(failure));
      return false;
    } finally {
      setSending(false);
    }
  };
  const submit = async () => {
    const words = text.trim();
    if (!words || sending) return;
    if (await send({ text: words, keys: ["Enter"] })) {
      setText("");
      box.current?.focus();
    }
  };
  return (
    <div className="composer">
      <div className="composer-keys" role="toolbar" aria-label="Keys">
        {KEYS.map((key) => (
          <button
            key={key.key}
            type="button"
            className="composer-key"
            title={key.title}
            disabled={sending}
            onClick={() => void send({ keys: [key.key] })}
          >
            {key.label}
          </button>
        ))}
      </div>
      {error && (
        <Text type="supporting" color="secondary" className="composer-error">
          {error}
        </Text>
      )}
      <div className="composer-row">
        <textarea
          ref={box}
          className="composer-box"
          rows={1}
          value={text}
          placeholder="Message the agent"
          aria-label="Message the agent"
          enterKeyHint="send"
          onChange={(event) => setText(event.target.value)}
          onKeyDown={(event) => {
            // A physical keyboard: Enter sends, Shift+Enter starts a new line.
            if (event.key === "Enter" && !event.shiftKey && !event.nativeEvent.isComposing) {
              event.preventDefault();
              void submit();
            }
          }}
        />
        <IconButton
          label="Send"
          variant="primary"
          size="md"
          isDisabled={!text.trim() || sending}
          onClick={() => void submit()}
          icon={<Icon icon="arrowUp" />}
        />
      </div>
    </div>
  );
}

export function AgentScreen({ agent, close }: { agent: Agent; close(): void }) {
  const viewport = useVisualViewport();
  const [mode, setMode] = useState<"transcript" | "terminal">("transcript");
  const [nudge, setNudge] = useState(0);
  useEffect(() => {
    const escape = (event: KeyboardEvent) => {
      if (event.key === "Escape" && !(event.target instanceof HTMLTextAreaElement)) close();
    };
    window.addEventListener("keydown", escape);
    return () => window.removeEventListener("keydown", escape);
  }, [close]);
  if (!agent.terminal) return null;
  return (
    <div className="agent-screen" style={viewport} role="dialog" aria-label={agent.name}>
      <header className="agent-screen-head">
        <IconButton
          label="Back"
          variant="ghost"
          size="sm"
          onClick={close}
          icon={<Icon icon="chevronLeft" />}
        />
        <div className="agent-screen-title">
          <Text type="label" weight="semibold" className="nowrap">
            {agent.name}
          </Text>
          <Status tone={agentTone(agent.state)} title={agentStates[agent.state]?.meaning}>
            {agent.state}
          </Status>
        </div>
        <SegmentedControl
          label="Show"
          size="sm"
          value={mode}
          onChange={(value) => setMode(value as "transcript" | "terminal")}
        >
          <SegmentedControlItem value="transcript" label="Transcript" />
          <SegmentedControlItem value="terminal" label="Terminal" />
        </SegmentedControl>
      </header>
      <main className="agent-screen-body">
        {mode === "transcript" ? (
          <Transcript terminal={agent.terminal} nudge={nudge} />
        ) : (
          <NativeTerminal key={agent.terminal} id={agent.terminal} />
        )}
      </main>
      <Composer terminal={agent.terminal} sent={() => setNudge((value) => value + 1)} />
    </div>
  );
}
