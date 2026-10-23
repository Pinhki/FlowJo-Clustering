(function () {
  const SPLIT_HANDLE_PX = 8;
  const sessionSplits = new Map();

  function clamp(value, min, max) {
    return Math.max(min, Math.min(max, value));
  }

  function splitForHandle(handle) {
    return handle && handle.closest(".Pinhki-split-resize, .gate-nav-split");
  }

  function splitMin(split, name, fallback) {
    const attr = split.dataset[name] || "";
    const cssNames = {
      splitMinLeft: "--Pinhki-split-min-left",
      splitMinRight: "--Pinhki-split-min-right",
      splitMinTop: "--Pinhki-split-min-top",
      splitMinBottom: "--Pinhki-split-min-bottom"
    };
    const cssName = cssNames[name];
    const css = getComputedStyle(split).getPropertyValue(cssName);
    const value = parseInt(attr || css, 10);
    return Number.isFinite(value) ? value : fallback;
  }

  function setSplitLeft(split, px) {
    split.style.setProperty("--Pinhki-split-left", `${px}px`);
    split.style.gridTemplateColumns = `minmax(0, var(--Pinhki-split-left)) ${SPLIT_HANDLE_PX}px minmax(0, 1fr)`;
    window.dispatchEvent(new Event("resize"));
  }

  function setSplitTop(split, px) {
    split.style.setProperty("--Pinhki-split-top", `${px}px`);
    split.style.gridTemplateRows = `minmax(0, var(--Pinhki-split-top)) ${SPLIT_HANDLE_PX}px minmax(0, 1fr)`;
    window.dispatchEvent(new Event("resize"));
  }

  function isVerticalSplit(split) {
    const style = getComputedStyle(split);
    return style.gridTemplateRows.split(" ").length >= 3 && style.gridTemplateColumns.split(" ").length < 3;
  }

  function splitSessionKey(split) {
    return split.id || split.dataset.splitKey || split.dataset.splitId || "default";
  }

  function restoreSessionSplit(split, vertical) {
    const remembered = sessionSplits.get(splitSessionKey(split));
    if (!remembered) return;
    if (vertical && Number.isFinite(remembered.top)) setSplitTop(split, remembered.top);
    if (!vertical && Number.isFinite(remembered.left)) setSplitLeft(split, remembered.left);
  }

  function updateScrollFade(el) {
    if (!el || !el.classList || !el.classList.contains("Pinhki-scroll-fade")) return;
    const maxScroll = Math.max(0, el.scrollHeight - el.clientHeight);
    const top = el.scrollTop > 1;
    const bottom = el.scrollTop < maxScroll - 1;
    el.classList.toggle("is-scroll-top", top);
    el.classList.toggle("is-scroll-bottom", bottom);
  }

  function updateAllScrollFades() {
    document.querySelectorAll(".Pinhki-scroll-fade").forEach(updateScrollFade);
  }

  function scheduleScrollFadeUpdate() {
    window.requestAnimationFrame(updateAllScrollFades);
  }

  document.addEventListener("scroll", e => {
    if (e.target && e.target.classList && e.target.classList.contains("Pinhki-scroll-fade")) updateScrollFade(e.target);
  }, true);
  window.addEventListener("resize", scheduleScrollFadeUpdate);
  document.addEventListener("shiny:value", scheduleScrollFadeUpdate);
  document.addEventListener("shiny:bound", scheduleScrollFadeUpdate);
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", scheduleScrollFadeUpdate);
  else scheduleScrollFadeUpdate();

  document.addEventListener("pointerdown", e => {
    const handle = e.target && e.target.closest(".Pinhki-split-resizer, .gate-nav-resizer");
    if (!handle) return;
    const split = splitForHandle(handle);
    if (!split) return;
    e.preventDefault();
    const vertical = isVerticalSplit(split);
    handle.setAttribute("aria-orientation", vertical ? "horizontal" : "vertical");
    const sessionKey = splitSessionKey(split);
    restoreSessionSplit(split, vertical);
    split.classList.add("is-drag-active");
    const move = ev => {
      const rect = split.getBoundingClientRect();
      const remembered = sessionSplits.get(sessionKey) || {};
      if (vertical) {
        const scale = (split.offsetHeight || rect.height || 1) / (rect.height || 1);
        const cssHeight = split.offsetHeight || rect.height * scale;
        const pointerY = (ev.clientY - rect.top) * scale;
        const minTop = splitMin(split, "splitMinTop", 240);
        const minBottom = splitMin(split, "splitMinBottom", 240);
        const top = clamp(pointerY, minTop, Math.max(minTop, cssHeight - minBottom));
        sessionSplits.set(sessionKey, Object.assign(remembered, { top }));
        setSplitTop(split, top);
      } else {
        const scale = (split.offsetWidth || rect.width || 1) / (rect.width || 1);
        const cssWidth = split.offsetWidth || rect.width * scale;
        const pointerX = (ev.clientX - rect.left) * scale;
        const minLeft = splitMin(split, "splitMinLeft", 260);
        const minRight = splitMin(split, "splitMinRight", 320);
        const left = clamp(pointerX, minLeft, Math.max(minLeft, cssWidth - minRight));
        sessionSplits.set(sessionKey, Object.assign(remembered, { left }));
        setSplitLeft(split, left);
      }
    };
    const up = () => {
      split.classList.remove("is-drag-active");
      window.removeEventListener("pointermove", move);
      window.removeEventListener("pointerup", up);
      window.removeEventListener("pointercancel", up);
    };
    window.addEventListener("pointermove", move);
    window.addEventListener("pointerup", up);
    window.addEventListener("pointercancel", up);
  });
})();
