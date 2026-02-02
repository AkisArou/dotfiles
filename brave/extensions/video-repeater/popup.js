// Popup script
const startTimeInput = document.getElementById("startTime");
const endTimeInput = document.getElementById("endTime");

// Load saved values from storage
window.addEventListener("load", () => {
  chrome.storage.local.get(["startTime", "endTime"], (data) => {
    if (data.startTime) startTimeInput.value = data.startTime;
    if (data.endTime) endTimeInput.value = data.endTime;
  });
});

document.getElementById("applyButton").addEventListener("click", async () => {
  const startTime = startTimeInput.value;
  const endTime = endTimeInput.value;

  if (!startTime || !endTime) {
    document.getElementById("status").textContent =
      "Please enter both start and end times";
    return;
  }

  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });

  chrome.tabs.sendMessage(
    tab.id,
    { action: "applyLoop", startTime, endTime },
    (response) => {
      if (response && response.status) {
        // Save values only on successful apply
        chrome.storage.local.set({ startTime, endTime });
        document.getElementById("status").textContent = response.message;
      } else {
        document.getElementById("status").textContent =
          "Error: " + (response?.error || "Unknown error");
      }
    },
  );
});

document.getElementById("stopButton").addEventListener("click", async () => {
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });

  chrome.tabs.sendMessage(tab.id, { action: "stopLoop" }, (response) => {
    if (response && response.status) {
      document.getElementById("status").textContent = response.message;
    }
  });
});
