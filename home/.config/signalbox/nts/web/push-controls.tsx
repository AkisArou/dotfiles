// Notifications on this device: the one-time offer above the views, and the
// switch the More sheet and Settings show.
import { Button } from "@astryxdesign/core/Button";
import { Text } from "@astryxdesign/core/Text";
import type { Push } from "./push.ts";

/** Offered once on a device that can turn them on: Turn on asks the system, Not now hides it. */
export function PushPrompt({ push, onDismiss }: { push: Push; onDismiss(): void }) {
  return (
    <div className="push-prompt" role="region" aria-label="Notifications">
      <div className="push-prompt-text">
        <Text type="body" weight="semibold">
          Get a notification when something needs you
        </Text>
        <Text type="supporting" color="secondary">
          Questions for you and issues waiting on your word, on this device.
        </Text>
      </div>
      <div className="push-prompt-actions">
        <Button
          label="Turn on notifications"
          variant="primary"
          size="sm"
          isDisabled={push.busy || !push.ready}
          onClick={() => void push.enable()}
        >
          Turn on
        </Button>
        <Button label="Not now" variant="ghost" size="sm" onClick={onDismiss}>
          Not now
        </Button>
      </div>
    </div>
  );
}

const SAYS: Record<Push["state"], string> = {
  on: "Notifications are on for this device.",
  off: "Notifications are off for this device.",
  blocked:
    "Notifications are blocked for this app. Allow them in the system settings (on an iPhone: Settings › Notifications › nts).",
  install:
    "To get notifications on an iPhone, add this page to the Home Screen (Share › Add to Home Screen) and open it from there.",
  unsupported: "This browser cannot show notifications.",
};

/** Where this device stands, and what can be done about it. */
export function PushSwitch({ push }: { push: Push }) {
  return (
    <div className="push-switch">
      <Text type="supporting" color="secondary">
        {push.error ? `${SAYS[push.state]} ${push.error}` : SAYS[push.state]}
      </Text>
      {push.state === "on" ? (
        <div className="push-prompt-actions">
          <Button
            label="Send a test notification"
            variant="secondary"
            size="sm"
            isDisabled={push.busy}
            onClick={() => void push.test()}
          >
            Send a test
          </Button>
          <Button
            label="Turn off notifications"
            variant="ghost"
            size="sm"
            isDisabled={push.busy}
            onClick={() => void push.disable()}
          >
            Turn off
          </Button>
        </div>
      ) : push.state === "off" ? (
        <Button
          label="Turn on notifications"
          variant="primary"
          size="sm"
          isDisabled={push.busy || !push.ready}
          onClick={() => void push.enable()}
        >
          Turn on
        </Button>
      ) : null}
    </div>
  );
}
