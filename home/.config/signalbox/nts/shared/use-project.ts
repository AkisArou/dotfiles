import { useCallback, useState } from "react";
import { useCommand, useConnection, useSource } from "signalbox/client/react";
import type { RemoteReading } from "signalbox/core/remote";
import { claimedIssues } from "./claims.ts";
import * as refs from "./model.ts";
import { useNts } from "../status/use-nts.ts";

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
  const settings = useSource(refs.settings),
    assignments = useSource(refs.assignments),
    needs = useSource(refs.needs);
  const setSettings = useCommand(refs.setSettings),
    pauseTasks = useCommand(refs.pauseTasks),
    resumeTasks = useCommand(refs.resumeTasks);
  const setAvailability = useCommand(refs.setAvailability),
    assignTask = useCommand(refs.assignTask),
    cancelAssignment = useCommand(refs.cancelAssignment),
    answerQuestion = useCommand(refs.answerQuestion),
    answerIssue = useCommand(refs.answerIssue),
    withdrawQuestion = useCommand(refs.withdrawQuestion);
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
      settings: current(settings),
      assignments: current(assignments) ?? [],
      needs: current(needs) ?? { questions: [], unasked: [], answered: [] },
    },
    commands: {
      startTerminal,
      setSettings,
      pauseTasks,
      resumeTasks,
      setAvailability,
      assignTask,
      cancelAssignment,
      answerQuestion,
      answerIssue,
      withdrawQuestion,
    },
    perform,
    error,
    dismissError: () => setError(undefined),
    busy:
      startTerminal.pending ||
      setSettings.pending ||
      pauseTasks.pending ||
      resumeTasks.pending ||
      setAvailability.pending ||
      assignTask.pending ||
      cancelAssignment.pending ||
      answerQuestion.pending ||
      answerIssue.pending ||
      withdrawQuestion.pending,
    online: connection.status === "connected",
  };
}
export type Project = ReturnType<typeof useProject>;
