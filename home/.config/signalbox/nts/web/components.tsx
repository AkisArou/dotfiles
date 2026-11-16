import type { ReactNode } from "react";
import { Button } from "@astryxdesign/core/Button";
import { Card } from "@astryxdesign/core/Card";
import { Badge as AstryxBadge } from "@astryxdesign/core/Badge";
import type { Project } from "../shared/use-project.ts";

export function Icon({ name }: { name: string }) {
  const paths: Record<string, ReactNode> = {
    Now: (
      <>
        <path d="m3 12 4-7 4 14 4-10 3 3h3" />
      </>
    ),
    Tasks: (
      <>
        <rect x="4" y="4" width="16" height="16" rx="3" />
        <path d="m8 9 1 1 2-2m2 1h3M8 15h2m3 0h3" />
      </>
    ),
    Agents: (
      <>
        <rect x="4" y="7" width="16" height="13" rx="3" />
        <path d="M12 3v4M8 12h1m6 0h1m-8 4h8" />
      </>
    ),
    Landing: (
      <>
        <path d="M12 3v12m-4-4 4 4 4-4M4 16v4h16v-4" />
      </>
    ),
    "Needs you": (
      <>
        <circle cx="12" cy="12" r="9" />
        <path d="M12 7v6m0 3h.01" />
      </>
    ),
    Plan: (
      <>
        <path d="M5 4h14v16H5zM8 8h8M8 12h8M8 16h5" />
      </>
    ),
    Benchmarks: <path d="M4 20h16M7 16v-5m5 5V4m5 12V8" />,
    Conformance: (
      <>
        <path d="M4 4h16v16H4zM8 9l2 2 5-5M8 16h8" />
      </>
    ),
    "Full check": (
      <>
        <circle cx="12" cy="12" r="9" />
        <path d="m7 12 3 3 7-7" />
      </>
    ),
    Machine: (
      <>
        <rect x="5" y="5" width="14" height="14" rx="2" />
        <path d="M9 9h6v6H9zM9 2v3m6-3v3m-6 14v3m6-3v3M2 9h3m-3 6h3m14-6h3m-3 6h3" />
      </>
    ),
    Health: <path d="M3 12h4l3-8 4 16 3-8h4" />,
  };
  return (
    <svg
      viewBox="0 0 24 24"
      width="18"
      height="18"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
    >
      {paths[name] ?? paths.Now}
    </svg>
  );
}
export function Badge({ children, state = "" }: { children: ReactNode; state?: string }) {
  const tone = /landed|passed|idle|ready|connected|^ok$/.test(state)
    ? "good"
    : /error|fail|red/.test(state)
      ? "bad"
      : /needs|held|parked|unscoped|amber/.test(state)
        ? "warn"
        : /build|review|merg|start|busy/.test(state)
          ? "active"
          : "neutral";
  return (
    <AstryxBadge
      className="badge"
      label={children}
      variant={
        tone === "good"
          ? "success"
          : tone === "bad"
            ? "error"
            : tone === "warn"
              ? "warning"
              : tone === "active"
                ? "info"
                : "neutral"
      }
    />
  );
}
export function Section({
  title,
  aside,
  children,
  className = "",
}: {
  title: string;
  aside?: ReactNode;
  children: ReactNode;
  className?: string;
}) {
  return (
    <Card className={`section ${className}`} padding={0}>
      <div className="section-heading">
        <h2>{title}</h2>
        {aside}
      </div>
      {children}
    </Card>
  );
}
export function Empty({ children }: { children: ReactNode }) {
  return <div className="empty">{children}</div>;
}
export function ago(time: number) {
  const seconds = Math.max(0, Math.floor((Date.now() - time) / 1000));
  return seconds < 60
    ? "just now"
    : seconds < 3600
      ? `${Math.floor(seconds / 60)}m ago`
      : `${Math.floor(seconds / 3600)}h ago`;
}
export function Action({
  project,
  children,
  onClick,
  primary = false,
  disabled = false,
}: {
  project: Project;
  children: string;
  onClick(): Promise<unknown>;
  primary?: boolean;
  disabled?: boolean;
}) {
  return (
    <Button
      label={children}
      variant={primary ? "primary" : "secondary"}
      size="lg"
      className={primary ? "button primary" : "button"}
      isDisabled={!project.online || project.busy || disabled}
      clickAction={async () => {
        await project.perform(onClick);
      }}
    >
      {children}
    </Button>
  );
}
