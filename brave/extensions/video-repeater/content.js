// Content script - runs in the context of web pages
let loopActive = false;
let loopStartTime = 0;
let loopEndTime = 0;
let currentVideo = null;

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

// Set up looping functionality
function setupLoop(video, startTime, endTime) {
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
    }
  } catch (error) {
    sendResponse({
      status: false,
      error: error.message,
    });
  }
});
