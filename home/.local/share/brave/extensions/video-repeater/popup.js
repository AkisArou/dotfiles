// Popup script
const startTimeInput = document.getElementById("startTime");
const endTimeInput = document.getElementById("endTime");
const statusEl = document.getElementById("status");
let statusTimeoutId = null;

function setStatus(message, { temporary = false } = {}) {
  if (statusTimeoutId) {
    clearTimeout(statusTimeoutId);
    statusTimeoutId = null;
  }

  statusEl.textContent = message || "";

  if (temporary && message) {
    statusTimeoutId = setTimeout(() => {
      statusEl.textContent = "";
      statusTimeoutId = null;
    }, 2000);
  }
}

// Load saved values from storage
window.addEventListener("load", () => {
  chrome.storage.local.get(
    ["startTime", "endTime", "pendingStartTime", "pendingEndTime"],
    (data) => {
      if (data.startTime) startTimeInput.value = data.startTime;
      if (data.endTime) endTimeInput.value = data.endTime;

      if (data.pendingStartTime || data.pendingEndTime) {
        const parts = [];
        if (data.pendingStartTime) {
          parts.push(`Pending start: ${data.pendingStartTime}`);
        }
        if (data.pendingEndTime) {
          parts.push(`Pending end: ${data.pendingEndTime}`);
        }
        setStatus(parts.join(" • "), { temporary: true });
      }
    },
  );
});

chrome.runtime.onMessage.addListener((request) => {
  if (request.action === "updateTimes") {
    if (request.startTime) startTimeInput.value = request.startTime;
    if (request.endTime) endTimeInput.value = request.endTime;
  } else if (request.action === "updatePending") {
    const parts = [];
    if (request.pendingStartTime) {
      parts.push(`Pending start: ${request.pendingStartTime}`);
    }
    if (request.pendingEndTime) {
      parts.push(`Pending end: ${request.pendingEndTime}`);
    }
    if (parts.length > 0) {
      setStatus(parts.join(" • "), { temporary: true });
    }
  } else if (request.action === "clearPending") {
    setStatus("");
  }
});

document.getElementById("applyButton").addEventListener("click", async () => {
  const startTime = startTimeInput.value;
  const endTime = endTimeInput.value;

  if (!startTime || !endTime) {
    setStatus("Please enter both start and end times");
    return;
  }

  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });

  chrome.tabs.sendMessage(
    tab.id,
    { action: "applyLoop", startTime, endTime },
    (response) => {
      if (response && response.status) {
        chrome.storage.local.set({ startTime, endTime });
        setStatus(response.message);
      } else {
        setStatus("Error: " + (response?.error || "Unknown error"));
      }
    },
  );
});

document.getElementById("stopButton").addEventListener("click", async () => {
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });

  chrome.tabs.sendMessage(tab.id, { action: "stopLoop" }, (response) => {
    if (response && response.status) {
      setStatus(response.message);
    }
  });
});

document.addEventListener("keydown", async (event) => {
  if (!event.ctrlKey || event.altKey || event.metaKey || event.shiftKey) return;

  if (event.key !== "," && event.key !== ".") return;

  event.preventDefault();

  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
  const action = event.key === "," ? "captureStart" : "captureEnd";

  chrome.tabs.sendMessage(tab.id, { action }, (response) => {
    if (response && response.status === false) {
      setStatus("Error: " + (response.error || "Unknown error"));
    }
  });
});
