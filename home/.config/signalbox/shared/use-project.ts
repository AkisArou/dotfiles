import { useCallback, useState } from "react";
import { useCommand, useConnection, useSource } from "signalbox/client/react";
import type { RemoteReading } from "signalbox/core/remote";
import { claimedIssues } from "./claims.ts";
import * as refs from "./model.ts";
import { useNts } from "../nts/use-nts.ts";

export function current<T>(reading: RemoteReading<T>): T | undefined {
  return reading.status === "ready"
    ? reading.value
    : reading.status === "error"
      ? reading.previous
      : undefined;
}
export function useProject() {
  const nts = useNts();
  const connection = useConnection();
  const overview = useSource(refs.overview),
    issues = useSource(refs.issues),
    agents = useSource(refs.agents),
    plan = useSource(refs.plan);
  const terminalPrograms = useSource(refs.terminalPrograms);
  const startTerminal = useCommand(refs.startTerminal);
  const [error, setError] = useState<string>();
  const perform = useCallback(async <T>(action: () => Promise<T>): Promise<T | undefined> => {
    setError(undefined);
    try {
      return await action();
    } catch (error) {
      setError(error instanceof Error ? error.message : String(error));
      return undefined;
    }
  }, []);
  return {
    nts,
    connection,
    overview,
    issues,
    agents,
    plan,
    terminalPrograms: current(terminalPrograms) ?? [],
    data: {
      overview: current(overview),
      issues: claimedIssues(current(issues) ?? [], current(agents) ?? [], [
        ...(nts.page("Machine")?.claims ?? []),
        ...(nts.page("Landing")?.claims ?? []),
      ]),
      agents: current(agents) ?? [],
      plan: current(plan),
    },
    commands: { startTerminal },
    perform,
    error,
    dismissError: () => setError(undefined),
    busy: startTerminal.pending,
    online: connection.status === "connected",
  };
}
export type Project = ReturnType<typeof useProject>;
