import { useEffect, useRef, useState } from "react";
import { Button } from "@astryxdesign/core/Button";
import { BottomSheet } from "@astryxdesign/core/BottomSheet";
import type { Tab } from "../shared/model.ts";
import { tabs } from "../shared/model.ts";
import { Icon } from "./components.tsx";

const primary = ["Now", "Tasks", "Agents", "Landing"] as const;
const extra = tabs.filter((name) => !primary.some((page) => page === name));
export function MobileNav({
  tab,
  select,
  pending,
}: {
  tab: Tab;
  select(tab: Tab): void;
  pending: number;
}) {
  const [open, setOpen] = useState(false);
  const trigger = useRef<HTMLButtonElement>(null);
  useEffect(() => {
    const resized = () => {
      if (window.innerWidth > 650) setOpen(false);
    };
    window.addEventListener("resize", resized);
    return () => window.removeEventListener("resize", resized);
  }, []);
  return (
    <>
      <nav className="mobile-bottom-nav" aria-label="Mobile project views">
        {primary.map((name) => (
          <Button
            key={name}
            label={name}
            variant="ghost"
            className={tab === name ? "mobile-nav-button active" : "mobile-nav-button"}
            aria-current={tab === name ? "page" : undefined}
            onClick={() => select(name)}
          >
            <span className="mobile-nav-content">
              <span className="mobile-nav-icon">
                <Icon name={name} />
                {name === "Now" && pending > 0 && (
                  <span className="mobile-nav-count">{pending}</span>
                )}
              </span>
              <span>{name}</span>
            </span>
          </Button>
        ))}
        <Button
          ref={trigger}
          label="More project views"
          variant="ghost"
          className={
            extra.some((name) => name === tab) ? "mobile-nav-button active" : "mobile-nav-button"
          }
          onClick={() => setOpen(true)}
          aria-haspopup="dialog"
        >
          <span className="mobile-nav-content">
            <span className="mobile-nav-icon">
              <svg
                viewBox="0 0 24 24"
                width="21"
                height="21"
                fill="currentColor"
                aria-hidden="true"
              >
                <circle cx="5" cy="12" r="1.6" />
                <circle cx="12" cy="12" r="1.6" />
                <circle cx="19" cy="12" r="1.6" />
              </svg>
            </span>
            <span>More</span>
          </span>
        </Button>
      </nav>
      <BottomSheet
        label="More views"
        isOpen={open}
        onOpenChange={setOpen}
        finalFocusRef={trigger}
        height="hug"
        purpose="info"
      >
        <div className="sheet-heading">
          <h2>More views</h2>
          <Button
            label="Close more views"
            icon={<span>×</span>}
            isIconOnly
            variant="ghost"
            onClick={() => setOpen(false)}
          />
        </div>
        <div className="sheet-choices">
          {extra.map((name) => (
            <Button
              key={name}
              label={name}
              variant={tab === name ? "secondary" : "ghost"}
              size="lg"
              width="100%"
              icon={<Icon name={name} />}
              onClick={() => {
                select(name);
                setOpen(false);
              }}
            />
          ))}
        </div>
      </BottomSheet>
    </>
  );
}
