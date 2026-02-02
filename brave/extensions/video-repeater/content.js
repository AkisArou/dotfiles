// Content script - runs in the context of web pages
let loopActive = false;
let loopStartTime = 0;
let loopEndTime = 0;
let currentVideo = null;
let pendingStartTime = null;
let pendingEndTime = null;

// Parse time string in format "min:sec" or "hour:min:sec"
function parseTimeString(timeStr) {
  const parts = timeStr.trim().split(":");
  let seconds = 0;

  if (parts.length === 2) {
    // min:sec
    seconds = parseInt(parts[0]) * 60 + parseInt(parts[1]);
  } else if (parts.length === 3) {
    // hour:min:sec
    seconds =
      parseInt(parts[0]) * 3600 + parseInt(parts[1]) * 60 + parseInt(parts[2]);
  } else {
    throw new Error("Invalid time format. Use min:sec or hour:min:sec");
  }

  return seconds;
}

function formatTime(seconds) {
  const totalSeconds = Math.max(0, Math.floor(seconds));
  const hours = Math.floor(totalSeconds / 3600);
  const minutes = Math.floor((totalSeconds % 3600) / 60);
  const secs = totalSeconds % 60;
  if (hours > 0) {
    return `${hours}:${String(minutes).padStart(2, "0")}:${String(secs).padStart(2, "0")}`;
  }
  return `${minutes}:${String(secs).padStart(2, "0")}`;
}

// Set up looping functionality
function setupLoop(video, startTime, endTime) {
  if (currentVideo) {
    currentVideo.removeEventListener("timeupdate", onTimeUpdate);
  }

  loopActive = true;
  loopStartTime = startTime;
  loopEndTime = endTime;
  currentVideo = video;

  video.addEventListener("timeupdate", onTimeUpdate);
  video.currentTime = startTime;
  video.play();
}

function onTimeUpdate() {
  if (!loopActive || !currentVideo) return;

  if (currentVideo.currentTime >= loopEndTime) {
    currentVideo.currentTime = loopStartTime;
    currentVideo.play();
  }
}

function stopLoop() {
  loopActive = false;
  if (currentVideo) {
    currentVideo.removeEventListener("timeupdate", onTimeUpdate);
    currentVideo.pause();
    currentVideo = null;
  }
}

function isEditableTarget(target) {
  if (!target) return false;
  const tagName = target.tagName?.toLowerCase();
  return (
    tagName === "input" || tagName === "textarea" || target.isContentEditable
  );
}

function applyPendingLoop(video) {
  if (pendingStartTime == null || pendingEndTime == null) return;
  if (pendingEndTime <= pendingStartTime) return;
  setupLoop(video, pendingStartTime, pendingEndTime);
  chrome.storage.local.remove(["pendingStartTime", "pendingEndTime"]);
  chrome.runtime.sendMessage({ action: "clearPending" }, () => {
    if (chrome.runtime.lastError) {
      // Popup likely not open; ignore.
    }
  });
}

function persistTimes(startSeconds, endSeconds) {
  const startTime = formatTime(startSeconds);
  const endTime = formatTime(endSeconds);

  chrome.storage.local.set({ startTime, endTime });
  chrome.runtime.sendMessage(
    { action: "updateTimes", startTime, endTime },
    () => {
      if (chrome.runtime.lastError) {
        // Popup likely not open; ignore.
      }
    },
  );
}

function persistPendingTimes() {
  const pendingStart =
    pendingStartTime == null ? null : formatTime(pendingStartTime);
  const pendingEnd = pendingEndTime == null ? null : formatTime(pendingEndTime);

  chrome.storage.local.set({
    pendingStartTime: pendingStart,
    pendingEndTime: pendingEnd,
  });
  chrome.runtime.sendMessage(
    {
      action: "updatePending",
      pendingStartTime: pendingStart,
      pendingEndTime: pendingEnd,
    },
    () => {
      if (chrome.runtime.lastError) {
        // Popup likely not open; ignore.
      }
    },
  );
}

function capturePendingStart(video) {
  pendingStartTime = video.currentTime;
  persistPendingTimes();
  if (pendingEndTime != null) {
    persistTimes(pendingStartTime, pendingEndTime);
  }
  applyPendingLoop(video);
}

function capturePendingEnd(video) {
  pendingEndTime = video.currentTime;
  persistPendingTimes();
  if (pendingStartTime != null) {
    persistTimes(pendingStartTime, pendingEndTime);
  }
  applyPendingLoop(video);
}

document.addEventListener("keydown", (event) => {
  if (!event.ctrlKey || event.altKey || event.metaKey || event.shiftKey) return;
  if (isEditableTarget(event.target)) return;

  const video = document.querySelector("video");
  if (!video) return;

  if (event.key === ",") {
    event.preventDefault();
    capturePendingStart(video);
  } else if (event.key === ".") {
    event.preventDefault();
    capturePendingEnd(video);
  }
});

chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
  try {
    if (request.action === "applyLoop") {
      const startTime = parseTimeString(request.startTime);
      const endTime = parseTimeString(request.endTime);

      if (endTime <= startTime) {
        sendResponse({
          status: false,
          error: "End time must be greater than start time",
        });
        return;
      }

      const video = document.querySelector("video");
      if (!video) {
        sendResponse({
          status: false,
          error: "No video element found on this page",
        });
        return;
      }

      setupLoop(video, startTime, endTime);
      persistTimes(startTime, endTime);
      sendResponse({
        status: true,
        message: `Loop applied: ${request.startTime} → ${request.endTime}`,
      });
    } else if (request.action === "stopLoop") {
      stopLoop();
      sendResponse({
        status: true,
        message: "Loop stopped",
      });
    } else if (
      request.action === "captureStart" ||
      request.action === "captureEnd"
    ) {
      const video = document.querySelector("video");
      if (!video) {
        sendResponse({
          status: false,
          error: "No video element found on this page",
        });
        return;
      }

      if (request.action === "captureStart") {
        capturePendingStart(video);
      } else {
        capturePendingEnd(video);
      }

      sendResponse({ status: true });
    }
  } catch (error) {
    sendResponse({
      status: false,
      error: error.message,
    });
  }
});
