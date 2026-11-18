// What you can do to a task, as the TUI's a offers it (@signalbox/kit/shared/actions): a
// button per action its state allows. One that needs words opens a short form;
// one whose note is optional runs at once, or with a note from "with a note".
import { useState } from "react";
import { Button } from "@astryxdesign/core/Button";
import { HStack } from "@astryxdesign/core/HStack";
import { Selector } from "@astryxdesign/core/Selector";
import { Text } from "@astryxdesign/core/Text";
import { TextArea } from "@astryxdesign/core/TextArea";
import { runTaskAction, taskActions } from "@signalbox/kit/shared/actions";
import type { ActionValues, TaskAction } from "@signalbox/kit/shared/actions";
import type { AssignmentView, Issue } from "../shared/model.ts";
import type { Project } from "../shared/use-project.ts";

const LEAD = new Set(["assign", "approve", "signoff", "retry"]);
const DANGER = new Set(["cancel", "refuse"]);

export function TaskActions({
  project,
  issue,
  task,
}: {
  project: Project;
  issue: Issue;
  task: AssignmentView | undefined;
}) {
  const actions = taskActions(issue, task, project.data.settings?.next);
  const [open, setOpen] = useState<TaskAction>();
  const [text, setText] = useState("");
  const [urgency, setUrgency] = useState("0");
  const [model, setModel] = useState("");
  const worker = project.data.settings?.roles.worker;
  const namedModels = open?.id === "assign" && worker?.pick === "coordinator" ? worker.models : [];
  const idle = !project.online || project.busy;
  const close = () => {
    setOpen(undefined);
    setText("");
    setUrgency("0");
    setModel("");
  };
  const run = async (action: TaskAction, values: ActionValues) => {
    const done = await project.perform(() =>
      runTaskAction(project.commands, action.id, issue.number, task, values),
    );
    if (done !== undefined) close();
  };
  if (!actions.length) return null;
  return (
    <div className="task-actions">
      <HStack gap={2} align="center" wrap="wrap">
        {actions.map((action) => (
          <Button
            key={action.id}
            label={`${action.label}: ${action.meaning}`}
            variant={
              LEAD.has(action.id) ? "primary" : DANGER.has(action.id) ? "destructive" : "secondary"
            }
            size="sm"
            isDisabled={idle}
            onClick={() =>
              action.input === "none" || action.input === "optional"
                ? void run(action, {})
                : setOpen(action)
            }
          >
            {action.label}
          </Button>
        ))}
        {actions.some((action) => action.input === "optional") && (
          <Button
            label="Add a note to an action"
            variant="ghost"
            size="sm"
            isDisabled={idle}
            onClick={() => setOpen(actions.find((action) => action.input === "optional"))}
          >
            With a note…
          </Button>
        )}
      </HStack>
      {open && (
        <div className="task-action-form">
          <Text type="supporting" color="secondary">
            {`${open.label}: ${open.meaning}`}
          </Text>
          {open.input === "urgency" ? (
            <Selector
              label="Urgency"
              size="sm"
              value={urgency}
              onChange={setUrgency}
              options={Array.from({ length: 19 }, (_, index) => 9 - index).map((level) => ({
                value: String(level),
                label: `${level > 0 ? "+" : ""}${level}${level === 0 ? " (the default)" : ""}`,
              }))}
            />
          ) : (
            <>
              {actions.filter((action) => action.input === "optional").length > 1 && (
                <Selector
                  label="Action"
                  size="sm"
                  value={open.id}
                  onChange={(id) => setOpen(actions.find((action) => action.id === id))}
                  options={actions
                    .filter((action) => action.input === "optional")
                    .map((action) => ({ value: action.id, label: action.label }))}
                />
              )}
              <TextArea
                label={open.input === "required" ? "Your words" : "A note (optional)"}
                value={text}
                onChange={setText}
                rows={4}
              />
            </>
          )}
          {namedModels.length > 1 && (
            <Selector
              label="Worker model"
              size="sm"
              value={model}
              onChange={setModel}
              placeholder="The first available model"
              options={namedModels.map((entry) => ({ value: entry.id, label: entry.label }))}
            />
          )}
          <HStack gap={2}>
            <Button
              label={open.label}
              variant={DANGER.has(open.id) ? "destructive" : "primary"}
              size="sm"
              isDisabled={idle || (open.input === "required" && !text.trim())}
              onClick={() =>
                void run(open, {
                  text,
                  urgency: Number(urgency),
                  ...(model ? { model } : {}),
                })
              }
            >
              {open.label}
            </Button>
            <Button label="Close the form" variant="ghost" size="sm" onClick={close}>
              Never mind
            </Button>
          </HStack>
        </div>
      )}
    </div>
  );
}
