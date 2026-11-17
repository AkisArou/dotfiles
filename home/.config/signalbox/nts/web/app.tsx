// The console's frame, laid out like the TUI: Astryx's AppShell with the views
// as tabs in the top bar (each with its key and, where it matters, a count) and
// a status bar at the bottom (the repository, how work runs, whether the host is
// live). The route below renders the view; ?issue=<n> opens an issue drawer
// over any of them.
import { useEffect, useState } from "react";
import type { ReactNode } from "react";
import { Outlet, useLocation, useNavigate, useSearch } from "@tanstack/react-router";
import { AppShell } from "@astryxdesign/core/AppShell";
import { Banner } from "@astryxdesign/core/Banner";
import { StatusDot } from "@astryxdesign/core/StatusDot";
import { Tab, TabList } from "@astryxdesign/core/TabList";
import { Text } from "@astryxdesign/core/Text";
import { Theme } from "@astryxdesign/core/theme";
import { TopNav, TopNavHeading } from "@astryxdesign/core/TopNav";
import { neutralTheme } from "@astryxdesign/theme-neutral/built";
import { useProject } from "../shared/use-project.ts";
import { needsCount, settingsLine, tabKey, tabs } from "../shared/model.ts";
import type { Tab as View } from "../shared/model.ts";
import { Empty } from "./components.tsx";
import { IssueDrawer } from "./issue-drawer.tsx";
import { AgentScreen } from "./agent-screen.tsx";
import { ProjectContext, viewOf, viewPath } from "./project.ts";
import { useNarrow } from "./narrow.ts";
import { AppearanceButton, TabBar } from "./tab-bar.tsx";
import { useOverlay } from "./overlay.ts";
import { PushContext, useAppBadge, usePush, usePushPrompt } from "./push.ts";
import { PushPrompt, PushSwitch } from "./push-controls.tsx";

type Mode = "dark" | "light" | "system";
const MODES: Mode[] = ["dark", "light", "system"];
const storedMode = (): Mode => {
  try {
    const value = localStorage.getItem("signalbox-nts-mode");
    return value === "light" || value === "system" ? value : "dark";
  } catch {
    return "dark";
  }
};

export function Shell() {
  const project = useProject();
  const navigate = useNavigate();
  const { pathname } = useLocation();
  const { issue, screen } = useSearch({ strict: false }) as { issue?: number; screen?: string };
  const tab = viewOf(pathname);
  const [mode, setMode] = useState<Mode>(storedMode);
  // A phone: the views in a bottom tab bar, the status line in its More sheet.
  const narrow = useNarrow();
  const [more, setMore] = useState(false);
  const overlay = useOverlay();
  // Notifications on this device: offered once, and the home-screen badge kept in step.
  const push = usePush();
  const pushPrompt = usePushPrompt(push.state);
  useAppBadge(needsCount(project.data.needs));
  // The home-screen app's status bar takes the header's colour (theme-color), in
  // either appearance, so it reads as one bar with it, as a native app's does.
  useEffect(() => {
    const frame = requestAnimationFrame(() => {
      const header = document.querySelector(".astryx-app-shell-header");
      const colour = header ? getComputedStyle(header).backgroundColor : "";
      const meta = document.querySelector('meta[name="theme-color"]');
      if (meta && colour && colour !== "rgba(0, 0, 0, 0)") meta.setAttribute("content", colour);
    });
    return () => cancelAnimationFrame(frame);
  }, [mode, narrow]);
  useEffect(() => {
    try {
      localStorage.setItem("signalbox-nts-mode", mode);
    } catch {
      // A private window keeps the choice for this visit only.
    }
  }, [mode]);
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
      if (index >= 0 && index < Math.min(10, tabs.length))
        void navigate({ to: viewPath[tabs[index]!] });
      if (event.key === "/") {
        event.preventDefault();
        void navigate({ to: "/tasks" }).then(() =>
          requestAnimationFrame(() => document.getElementById("task-search")?.focus()),
        );
      }
    };
    window.addEventListener("keydown", keydown);
    return () => window.removeEventListener("keydown", keydown);
  }, [navigate]);
  const data = project.data;
  const sourceErrors = Object.entries({
    Tasks: project.issues,
    Agents: project.agents,
    Plan: project.plan,
    ...project.nts.readings,
  }).filter(([, reading]) => reading.status === "error");
  const live = data.agents.filter((agent) => agent.state !== "ended").length;
  const counts: Partial<Record<View, ReactNode>> = {
    "Needs you": needsCount(data.needs) ? (
      <span className="tab-count attention">{needsCount(data.needs)}</span>
    ) : undefined,
    Agents: <span className="tab-count">{live}</span>,
  };
  const banners = [
    !project.online && (
      <Banner
        key="offline"
        status="warning"
        container="section"
        title={`Host ${project.connection.status}`}
        description="Showing the last readings; controls return when the connection does."
      />
    ),
    data.settings?.paused && (
      <Banner
        key="paused"
        status="warning"
        container="section"
        title="New tasks are paused"
        description={data.settings.paused.reason}
      />
    ),
    data.settings?.error && (
      <Banner
        key="settings"
        status="error"
        container="section"
        title="settings.json is not in force; the last good settings are"
        description={data.settings.error}
      />
    ),
    project.error && (
      <Banner
        key="error"
        status="error"
        container="section"
        title={project.error}
        isDismissable
        onDismiss={project.dismissError}
      />
    ),
    sourceErrors.length > 0 && (
      <Banner
        key="sources"
        status="error"
        container="section"
        title={`${sourceErrors.length} source${sourceErrors.length === 1 ? "" : "s"} unavailable; their last readings may be stale`}
        description={sourceErrors
          .map(
            ([name, reading]) =>
              `${name}: ${reading.status === "error" ? reading.error.message : ""}`,
          )
          .join(" · ")}
      />
    ),
  ].filter(Boolean);
  const next = MODES[(MODES.indexOf(mode) + 1) % MODES.length]!;
  const appearance = <AppearanceButton mode={mode} next={next} onSwitch={() => setMode(next)} />;
  const status = [
    data.overview?.repo,
    data.settings && settingsLine(data.settings),
    `${data.issues.length} open issues`,
    `${live} live agent${live === 1 ? "" : "s"}`,
  ]
    .filter(Boolean)
    .join(" · ");
  const liveDot = (
    <span className="statusbar-live">
      <StatusDot
        variant={project.online ? "success" : "warning"}
        label={project.online ? "Live" : project.connection.status}
        isPulsing={!project.online}
      />
      <Text type="supporting" color="secondary">
        {project.online ? "live" : project.connection.status}
      </Text>
    </span>
  );
  return (
    <ProjectContext value={project}>
      <PushContext value={push}>
        <Theme theme={neutralTheme} mode={mode}>
          <AppShell
            height="fill"
            // The root (#root) is pinned to the screen; the shell fills it, not 100dvh.
            style={{ height: "100%", flex: 1, minHeight: 0 }}
            contentPadding={0}
            mobileNav={false}
            banner={banners.length ? <div className="banners">{banners}</div> : undefined}
            topNav={
              <TopNav
                label="Views"
                heading={<TopNavHeading heading={narrow ? tab : "nts"} />}
                startContent={
                  narrow ? undefined : (
                    <TabList
                      value={tab}
                      onChange={(value) => void navigate({ to: viewPath[value as View] })}
                      size="sm"
                    >
                      {tabs.map((name, index) => {
                        const key = tabKey(index);
                        return (
                          <Tab
                            key={name}
                            value={name}
                            label={name}
                            icon={key ? <span className="tab-key">{key}</span> : undefined}
                            endContent={counts[name]}
                          />
                        );
                      })}
                    </TabList>
                  )
                }
                endContent={narrow ? liveDot : appearance}
              />
            }
          >
            <div className={narrow ? "frame narrow" : "frame"}>
              <div className="view">
                <div className="view-body">
                  {data.overview && pushPrompt.show && push.ready && (
                    <PushPrompt push={push} onDismiss={pushPrompt.dismiss} />
                  )}
                  {!data.overview ? <Empty>Connecting to the project…</Empty> : <Outlet />}
                </div>
              </div>
              {narrow ? (
                <TabBar
                  view={tab}
                  counts={{ "Needs you": needsCount(data.needs), Agents: live }}
                  more={more}
                  setMore={setMore}
                  status={status}
                  appearance={appearance}
                  notifications={<PushSwitch push={push} />}
                />
              ) : (
                <footer className="statusbar">
                  <Text type="supporting" color="secondary" className="statusbar-line">
                    {status}
                  </Text>
                  {liveDot}
                </footer>
              )}
            </div>
            {screen && project.data.agents.find((agent) => agent.id === screen) && (
              <AgentScreen
                agent={project.data.agents.find((agent) => agent.id === screen)!}
                close={() => overlay.close("screen")}
              />
            )}
            <IssueDrawer project={project} number={issue} close={() => overlay.close("issue")} />
          </AppShell>
        </Theme>
      </PushContext>
    </ProjectContext>
  );
}
