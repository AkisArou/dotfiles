import { useEffect, useState } from "react";
import type { ReactNode } from "react";
import { Button } from "@astryxdesign/core/Button";
import { SideNavItem } from "@astryxdesign/core/SideNav";
import { Theme } from "@astryxdesign/core/theme";
import { neutralTheme } from "@astryxdesign/theme-neutral/built";
import { useProject } from "../shared/use-project.ts";
import { tabs, tabKey } from "../shared/model.ts";
import type { Tab } from "../shared/model.ts";
import { Empty, Icon } from "./components.tsx";
import { Now } from "./views/now.tsx";
import { Tasks } from "./views/tasks.tsx";
import { Agents } from "./views/agents.tsx";
import { UserDecisions } from "./views/orchestration.tsx";
import { Plan } from "./views/plan.tsx";
import { MobileNav } from "./mobile-nav.tsx";
import { NtsView, NtsHealth } from "./views/nts.tsx";

interface InstallPrompt extends Event {
  prompt(): Promise<void>;
  userChoice: Promise<{ outcome: "accepted" | "dismissed" }>;
}
function InstallButton() {
  const [prompt, setPrompt] = useState<InstallPrompt>();
  useEffect(() => {
    const available = (event: Event) => {
      event.preventDefault();
      setPrompt(event as InstallPrompt);
    };
    const installed = () => setPrompt(undefined);
    window.addEventListener("beforeinstallprompt", available);
    window.addEventListener("appinstalled", installed);
    return () => {
      window.removeEventListener("beforeinstallprompt", available);
      window.removeEventListener("appinstalled", installed);
    };
  }, []);
  if (!prompt || window.matchMedia("(display-mode: standalone)").matches) return null;
  return (
    <Button
      label="Install app"
      variant="secondary"
      className="button install-button"
      onClick={() => {
        void (async () => {
          try {
            await prompt.prompt();
            await prompt.userChoice;
          } finally {
            setPrompt(undefined);
          }
        })().catch(console.error);
      }}
    >
      Install app ↗
    </Button>
  );
}

export function App() {
  const project = useProject();
  const [tab, setTab] = useState<Tab>("Now");
  const [theme, setTheme] = useState(() =>
    localStorage.getItem("signalbox-nts-theme") === "paper" ? "paper" : "night",
  );
  useEffect(() => {
    localStorage.setItem("signalbox-nts-theme", theme);
  }, [theme]);
  useEffect(() => {
    const keydown = (event: KeyboardEvent) => {
      const target = event.target;
      if (
        target instanceof HTMLElement &&
        (target.isContentEditable ||
          /INPUT|TEXTAREA|SELECT/.test(target.tagName) ||
          target.closest(
            'dialog, [role="dialog"], [role="listbox"], [role="combobox"], [role="menu"], .terminal-screen',
          ))
      )
        return;
      if (event.ctrlKey || event.metaKey || event.altKey) return;
      const index = event.key === "0" ? 9 : Number(event.key) - 1;
      if (index >= 0 && index < tabs.length) setTab(tabs[index]!);
      if (event.key === "/") {
        event.preventDefault();
        setTab("Tasks");
        requestAnimationFrame(() => document.getElementById("task-search")?.focus());
      }
    };
    window.addEventListener("keydown", keydown);
    return () => window.removeEventListener("keydown", keydown);
  }, []);
  const data = project.data;
  const sourceErrors = Object.entries({
    Tasks: project.issues,
    Agents: project.agents,
    Plan: project.plan,
    ...project.nts.readings,
  }).filter(([, reading]) => reading.status === "error");
  const pages: Record<Tab, ReactNode> = {
    Now: <Now project={project} navigate={setTab} />,
    Tasks: <Tasks project={project} />,
    "Needs you": (
      <>
        <UserDecisions project={project} />
        <Tasks project={project} needsYou />
      </>
    ),
    Agents: <Agents project={project} />,
    Landing: <NtsView project={project} page="Landing" />,
    Plan: <Plan project={project} />,
    Benchmarks: <NtsView project={project} page="Benchmarks" />,
    Conformance: <NtsView project={project} page="Conformance" />,
    "Full check": <NtsView project={project} page="Full check" />,
    Machine: <NtsView project={project} page="Machine" />,
    Health: <NtsHealth project={project} />,
  };
  return (
    <Theme theme={neutralTheme} mode={theme === "paper" ? "light" : "dark"}>
      <div className="app-shell">
        <a href="#content" className="skip-link">
          Skip to content
        </a>
        <aside className="sidebar">
          <a
            className="brand"
            href="#"
            onClick={(event) => {
              event.preventDefault();
              setTab("Now");
            }}
          >
            <span className="brand-mark">
              s<span>·</span>
            </span>
            <span>
              signalbox<span className="brand-version">nts</span>
            </span>
          </a>
          <div className="project-switch">
            <span className="project-symbol">n</span>
            <div>
              <strong>nts</strong>
              <span>Compiler & runtime</span>
            </div>
            <span className="chevron">⌄</span>
          </div>
          <div className="nav-label">WORKSPACE</div>
          <nav aria-label="Project views">
            {tabs.map((name, index) => (
              <SideNavItem
                label={name}
                isSelected={name === tab}
                key={name}
                className={`nav-item ${name === tab ? "selected" : ""}`}
                onClick={() => setTab(name)}
                icon={<Icon name={name} />}
                endContent={
                  name === "Needs you" &&
                  data.issues.some((issue) => issue.state === "needs-user") ? (
                    <span className="nav-count">
                      {data.issues.filter((issue) => issue.state === "needs-user").length}
                    </span>
                  ) : (
                    <kbd>{tabKey(index)}</kbd>
                  )
                }
              />
            ))}
          </nav>
          <div className="sidebar-bottom">
            <div className="project-tag">
              <span className="status-dot" />
              NTS
            </div>
            <p>
              GitHub issues. Native agents.
              <br />
              Your NTS tooling and services.
            </p>
            <Button
              label={theme === "night" ? "Light appearance" : "Dark appearance"}
              variant="ghost"
              className="theme-button"
              onClick={() => setTheme(theme === "night" ? "paper" : "night")}
            >
              {theme === "night" ? "◐ Paper theme" : "◑ Night theme"}
            </Button>
          </div>
        </aside>
        <div className="workspace">
          <header className="topbar">
            <div className="breadcrumb">
              nts <span>/</span> <strong>{tab}</strong>
            </div>
            <div className="topbar-right">
              <InstallButton />
              <span className={`connection ${project.online ? "online" : "offline"}`} role="status">
                <span className="status-dot" />
                {project.connection.status === "connected" ? "Live" : project.connection.status}
              </span>
              <span className="divider" />
              <span>SELF-HOSTED</span>
              <span className="avatar">AK</span>
            </div>
          </header>
          <main id="content" tabIndex={-1}>
            <div className="page-header">
              <div>
                <div className="eyebrow">PROJECT CONTROL ROOM</div>
                <h1>
                  {
                    {
                      Now: "What’s moving.",
                      Tasks: "The work ahead.",
                      Agents: "Your working sessions.",
                      Landing: "NTS’s landing service.",
                      "Needs you": "Your open questions.",
                      Plan: "Where we’re going.",
                      Benchmarks: "What NTS measured.",
                      Conformance: "Coverage and recorded outcomes.",
                      "Full check": "From landed to verified.",
                      Machine: "NTS machine admission.",
                      Health: "What needs attention in NTS.",
                    }[tab]
                  }
                </h1>
                <p>
                  {
                    {
                      Now: "A live view of the work, the workers, and what needs your attention.",
                      Tasks: "GitHub issues organized by NTS streams, groups and state.",
                      "Needs you": "Open GitHub issues labelled state:needs-user.",
                      Agents:
                        "Native Codex and Claude Code terminals, including configured existing sessions.",
                      Landing: "Queue and recent answers from NTS’s own tooling.",
                      Plan: "The project’s own plan.md, independent of Signalbox.",
                      Benchmarks:
                        "Run identity, comparator versions, verdict counts and per-case ratios.",
                      Conformance:
                        "Whole-suite census coverage alongside the records each backend is held to.",
                      "Full check":
                        "Core and wide runs, culprits, records, inventory findings and outstanding task lines.",
                      Machine:
                        "Memory reservations and waits, worktree pool capacity, measurement locks and services.",
                      Health:
                        "NTS-specific thresholds and explanations, collected from its observations.",
                    }[tab]
                  }
                </p>
              </div>
              <span className="subtle">{data.overview?.repo}</span>
            </div>
            {!project.online && (
              <div className="notice warning" role="status">
                Host {project.connection.status}. Showing the last received snapshots; controls will
                return when the connection recovers.
              </div>
            )}
            {project.error && (
              <div className="notice danger" role="alert">
                <span>{project.error}</span>
                <Button
                  label="Dismiss error"
                  icon={<span>×</span>}
                  isIconOnly
                  variant="ghost"
                  onClick={project.dismissError}
                />
              </div>
            )}
            {sourceErrors.length > 0 && (
              <details className="notice danger">
                <summary>
                  {sourceErrors.length} project source(s) unavailable · retained readings may be
                  stale
                </summary>
                {sourceErrors.map(([name, reading]) => (
                  <p key={name}>
                    {name}: {reading.status === "error" && reading.error.message}
                  </p>
                ))}
              </details>
            )}
            {!data.overview ? <Empty>Connecting to the project…</Empty> : pages[tab]}
          </main>
          <footer className="footer">
            <span>
              <span className="status-dot" /> nts <span className="footer-separator">/</span>{" "}
              {data.overview?.repo}
            </span>
            <span>
              <kbd>1–9 / 0</kbd> switch view <kbd>/</kbd> find a task
            </span>
          </footer>
        </div>
        <MobileNav
          tab={tab}
          select={setTab}
          pending={data.issues.filter((issue) => issue.state === "needs-user").length}
        />
      </div>
    </Theme>
  );
}
