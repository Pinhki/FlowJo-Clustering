(function () {
  "use strict";

  const state = new Map();
  const palette = ["#2563eb", "#dc2626", "#16a34a", "#9333ea", "#ea580c", "#0891b2", "#be123c"];
  const pendingTimers = new Map();

  function finite(v) { return Number.isFinite(Number(v)); }
  function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }
  function input(id, value) {
    if (window.Shiny && id) Shiny.setInputValue(id, value, { priority: "event" });
  }
  function draftPayload(s, value) {
    return Object.assign({ plot: s.payload.plot || "", parentId: s.payload.parentId || "root", channels: s.payload.channels || {} }, value);
  }

  function setup(payload) {
    const wrap = document.getElementById(payload.id);
    if (!wrap) {
      retrySetup(payload);
      return null;
    }
    const points = wrap.querySelector(".gate-canvas-points");
    const overlay = wrap.querySelector(".gate-canvas-overlay");
    if (!points || !overlay) {
      retrySetup(payload);
      return null;
    }
    let s = state.get(payload.id);
    if (!s) {
      s = { wrap, points, overlay, payload: null, draft: null, drag: null, closed: false, prevPolygonDown: null, lastPolygonDown: null };
      state.set(payload.id, s);
      bind(s);
    } else if (s.wrap !== wrap || s.points !== points || s.overlay !== overlay) {
      s.wrap = wrap;
      s.points = points;
      s.overlay = overlay;
      s.drag = null;
      bind(s);
    }
    if (s.drag) return s;
    payload.polygon = asPoints(payload.polygon || []);
    wrap.removeAttribute("data-flowjo-canvas-ready");
    if (points) points.removeAttribute("data-flowjo-canvas-ready");
    if (overlay) overlay.removeAttribute("data-flowjo-canvas-ready");
    wrap.removeAttribute("data-flowjo-canvas-ready-token");
    if (points) points.removeAttribute("data-flowjo-canvas-ready-token");
    if (overlay) overlay.removeAttribute("data-flowjo-canvas-ready-token");
    const previousPayload = s.payload;
    if (payload.pointDataUnchanged && previousPayload) {
      // Agent note: Gate-only server updates omit large point arrays; retain the last point layer while applying fresh overlays and counts.
      for (const key of ["x", "y", "marker", "group", "groupColors"]) payload[key] = previousPayload[key];
    }
    const previousPointKey = pointLayerKey(previousPayload);
    s.payload = payload;
    const resized = sizeCanvases(s);
    if (!resized && previousPointKey && previousPointKey === pointLayerKey(payload)) drawOverlay(s);
    else draw(s);
    scheduleCanvasRemeasure();
    updateCursor(s);
    wrap.setAttribute("data-flowjo-canvas-ready", "true");
    points.setAttribute("data-flowjo-canvas-ready", "true");
    overlay.setAttribute("data-flowjo-canvas-ready", "true");
    const readyToken = window.FlowJoCanvasReadyTokens && window.FlowJoCanvasReadyTokens[payload.id];
    if (readyToken) {
      wrap.setAttribute("data-flowjo-canvas-ready-token", readyToken);
      points.setAttribute("data-flowjo-canvas-ready-token", readyToken);
      overlay.setAttribute("data-flowjo-canvas-ready-token", readyToken);
    }
    clearPending(wrap);
    // Agent note: Acknowledge this payload after drawing; the server rejects stale tokens.
    if (payload.readyInput && payload.readyToken && window.Shiny) {
      const readyInput = payload.readyInput;
      const readyToken = payload.readyToken;
      requestAnimationFrame(function () {
        window.Shiny.setInputValue(readyInput, { token: readyToken }, { priority: "event" });
      });
    }
    return s;
  }

  function clearPending(wrap) {
    if (!wrap) return;
    const timer = pendingTimers.get(wrap.id);
    if (timer) clearTimeout(timer);
    pendingTimers.delete(wrap.id);
    wrap.classList.remove("is-plot-pending");
    const pending = wrap.querySelector(".Pinhki-plot-pending");
    if (pending) pending.setAttribute("aria-hidden", "true");
  }

  function queuePending(wrap) {
    if (!wrap || wrap.classList.contains("blank")) return;
    const oldTimer = pendingTimers.get(wrap.id);
    if (oldTimer) clearTimeout(oldTimer);
    const timer = setTimeout(() => {
      pendingTimers.delete(wrap.id);
      if (!wrap.isConnected) return;
      wrap.classList.add("is-plot-pending");
      const pending = wrap.querySelector(".Pinhki-plot-pending");
      if (pending) pending.setAttribute("aria-hidden", "false");
    }, 140);
    pendingTimers.set(wrap.id, timer);
  }


  function retrySetup(payload) {
    const tries = payload._tries || 0;
    if (tries >= 10) return;
    requestAnimationFrame(() => setup(Object.assign({}, payload, { _tries: tries + 1 })));
  }

  function sizeCanvases(s, force) {
    const rect = s.wrap.getBoundingClientRect();
    // Agent note: The app shell is visually scaled; rect dimensions already include
    // that scale and would be scaled a second time when assigned as CSS pixels.
    const w = Math.max(1, Math.round(s.wrap.offsetWidth || rect.width || 1));
    const h = Math.max(1, Math.round(s.wrap.offsetHeight || rect.height || 1));
    const dpr = window.devicePixelRatio || 1;
    // Agent note: Tick labels and the rotated title need separate horizontal space,
    // including when an axis update changes label widths without resizing the card.
    const measure = s.points.getContext("2d");
    measure.font = "13px system-ui, sans-serif";
    const labels = axisTicks(s.payload, "y").labels;
    const tickWidth = Math.max(0, ...labels.map(label => measure.measureText(String(label || "")).width));
    const ml = s.payload.hideAxisText ? 58 : Math.max(58, Math.ceil(tickWidth) + 42);
    const next = { w, h, ml, mr: 18, mt: 58, mb: 46 };
    if (!force && s.box && s.box.w === next.w && s.box.h === next.h && s.box.ml === next.ml && s.dpr === dpr) return false;
    for (const c of [s.points, s.overlay]) {
      c.width = Math.max(1, Math.floor(w * dpr));
      c.height = Math.max(1, Math.floor(h * dpr));
      c.style.width = w + "px";
      c.style.height = h + "px";
      const ctx = c.getContext("2d");
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    }
    s.box = next;
    s.dpr = dpr;
    return true;
  }

  function remeasureAllCanvases() {
    // Agent note: Shiny plot switches settle layout after payload messages; force the CSS-pixel canvas box before redrawing.
    state.forEach(s => {
      if (!s.payload) return;
      sizeCanvases(s, true);
      draw(s);
    });
  }

  function scheduleCanvasRemeasure() {
    requestAnimationFrame(() => requestAnimationFrame(remeasureAllCanvases));
  }

  function pointLayerKey(p) {
    if (!p) return null;
    // Agent note: Keep this key tied to point-layer inputs only; gate selection must stay overlay-only for fast focus changes.
    return JSON.stringify({
      blank: !!p.blank,
      plot: p.plot || "",
      parentId: p.parentId || "",
      mode: p.mode || "dot",
      channels: p.channels || {},
      ranges: p.ranges || {},
      labels: p.labels || {},
      ticks: p.ticks || {},
      x: arraySignature(p.x),
      y: arraySignature(p.y),
      marker: p.mode === "marker" ? arraySignature(p.marker) : null,
      group: p.mode === "gate" ? arraySignature(p.group) : null
    });
  }

  function arraySignature(values) {
    if (!Array.isArray(values)) return null;
    const n = values.length;
    if (!n) return "0";
    const idx = [0, Math.floor(n / 7), Math.floor(n / 5), Math.floor(n / 3), Math.floor(n / 2), Math.floor(n * 2 / 3), Math.floor(n * 4 / 5), Math.floor(n * 6 / 7), n - 1];
    return String(n) + ":" + idx.map(i => sigValue(values[clamp(i, 0, n - 1)])).join(",");
  }

  function sigValue(value) {
    const n = Number(value);
    return Number.isFinite(n) ? n.toPrecision(8) : String(value);
  }

  function xScale(s, x) {
    const r = s.payload.ranges.x;
    return s.box.ml + (Number(x) - r[0]) / (r[1] - r[0] || 1) * (s.box.w - s.box.ml - s.box.mr);
  }
  function yScale(s, y) {
    const r = s.payload.ranges.y;
    return s.box.h - s.box.mb - (Number(y) - r[0]) / (r[1] - r[0] || 1) * (s.box.h - s.box.mt - s.box.mb);
  }
  function eventXScale(s, x) {
    const r = s.payload.ranges.x;
    return xScale(s, clamp(Number(x), r[0], r[1]));
  }
  function eventYScale(s, y) {
    const r = s.payload.ranges.y;
    return yScale(s, clamp(Number(y), r[0], r[1]));
  }
  function xData(s, px) {
    const r = s.payload.ranges.x;
    return r[0] + (px - s.box.ml) / (s.box.w - s.box.ml - s.box.mr || 1) * (r[1] - r[0]);
  }
  function yData(s, py) {
    const r = s.payload.ranges.y;
    return r[0] + (s.box.h - s.box.mb - py) / (s.box.h - s.box.mt - s.box.mb || 1) * (r[1] - r[0]);
  }

  function draw(s) {
    if (!s.payload) return;
    drawPoints(s);
    drawOverlay(s);
  }

  function drawPoints(s) {
    const ctx = s.points.getContext("2d");
    const p = s.payload;
    ctx.clearRect(0, 0, s.box.w, s.box.h);
    ctx.fillStyle = "#ffffff";
    ctx.fillRect(0, 0, s.box.w, s.box.h);
    if (p.blank) {
      ctx.fillStyle = "#6b7280";
      ctx.font = "13px system-ui, sans-serif";
      ctx.textAlign = "center";
      ctx.fillText("No parent population", s.box.w / 2, s.box.h / 2);
      return;
    }
    if (p.mode === "pseudocolor" || p.mode === "contour") drawBins(s, p.mode);
    else drawDots(s);
    drawAxes(s, ctx);
  }

  function drawDots(s) {
    const ctx = s.points.getContext("2d");
    const p = s.payload;
    const m = p.marker || [];
    const finiteM = m.filter(finite);
    const minM = Math.min(...finiteM, 0);
    const maxM = Math.max(...finiteM, 1);
    for (let i = 0; i < p.x.length; i++) {
      if (!finite(p.x[i]) || !finite(p.y[i])) continue;
      let col = "#2f3a45";
      if (p.mode === "marker" && finite(m[i])) col = ramp((m[i] - minM) / (maxM - minM || 1));
      if (p.mode === "gate") col = groupColor(p, p.group && p.group[i]);
      ctx.fillStyle = col;
      ctx.globalAlpha = p.mode === "dot" ? 0.35 : 0.65;
      // Agent note: Events outside a fixed viewport accumulate at its edge; gate geometry deliberately continues to use unclamped xScale/yScale.
      ctx.fillRect(eventXScale(s, p.x[i]), eventYScale(s, p.y[i]), 1.6, 1.6);
    }
    ctx.globalAlpha = 1;
  }

  function drawBins(s, mode) {
    const ctx = s.points.getContext("2d");
    const p = s.payload;
    const plotW = s.box.w - s.box.ml - s.box.mr;
    const plotH = s.box.h - s.box.mt - s.box.mb;
    // Agent note: Density bins are fixed in data space so resizing/export cannot change density normalization.
    const nx = 512;
    const ny = 512;
    const bins = new Uint32Array(nx * ny);
    for (let i = 0; i < p.x.length; i++) {
      if (!finite(p.x[i]) || !finite(p.y[i])) continue;
      const xi = clamp(Math.floor((p.x[i] - p.ranges.x[0]) / (p.ranges.x[1] - p.ranges.x[0] || 1) * nx), 0, nx - 1);
      const yi = clamp(Math.floor((p.y[i] - p.ranges.y[0]) / (p.ranges.y[1] - p.ranges.y[0] || 1) * ny), 0, ny - 1);
      bins[yi * nx + xi]++;
    }
    let max = 1;
    for (const b of bins) if (b > max) max = b;
    const pw = plotW / nx;
    const ph = plotH / ny;
    for (let yi = 0; yi < ny; yi++) {
      for (let xi = 0; xi < nx; xi++) {
        const v = bins[yi * nx + xi];
        if (!v) continue;
        const t = Math.log1p(v) / Math.log1p(max);
        if (mode === "contour") {
          if (t < 0.18 || Math.abs((t * 10) % 1) > 0.13) continue;
          ctx.fillStyle = "#2f3a45";
          ctx.globalAlpha = 0.55;
        } else {
          ctx.fillStyle = ramp(t);
          ctx.globalAlpha = 0.9;
        }
        ctx.fillRect(s.box.ml + xi * pw, s.box.h - s.box.mb - (yi + 1) * ph, pw, ph);
      }
    }
    ctx.globalAlpha = 1;
  }

  function drawAxes(s, ctx) {
    const b = s.box, p = s.payload;
    ctx.strokeStyle = "#111827";
    ctx.lineWidth = 1.5;
    ctx.beginPath();
    ctx.moveTo(b.ml, b.mt);
    ctx.lineTo(b.ml, b.h - b.mb);
    ctx.lineTo(b.w - b.mr, b.h - b.mb);
    ctx.stroke();
    ctx.fillStyle = "#111827";
    ctx.font = "13px system-ui, sans-serif";
    ctx.textAlign = "center";
    if (!p.hideAxisText) ctx.fillText(axisTitle(p, "x"), (b.w + b.ml - b.mr) / 2, b.h - 12);
    ctx.save();
    ctx.translate(16, (b.h + b.mt - b.mb) / 2);
    ctx.rotate(-Math.PI / 2);
    if (!p.hideAxisText) ctx.fillText(axisTitle(p, "y"), 0, 0);
    ctx.restore();
    ctx.font = "11px system-ui, sans-serif";
    ctx.strokeStyle = "#9ca3af";
    ctx.lineWidth = 0.75;
    for (const tx of axisTicks(p, "x").minor) {
      const px = xScale(s, tx);
      if (px < b.ml || px > b.w - b.mr) continue;
      ctx.textAlign = "center";
      ctx.beginPath();
      ctx.moveTo(px, b.h - b.mb);
      ctx.lineTo(px, b.h - b.mb + 4);
      ctx.stroke();
    }
    ctx.strokeStyle = "#111827";
    ctx.lineWidth = 1.5;
    axisTicks(p, "x").major.forEach((tx, i) => {
      const px = xScale(s, tx);
      if (px < b.ml || px > b.w - b.mr) return;
      const label = axisTicks(p, "x").labels[i] ?? fmt(tx);
      ctx.beginPath();
      ctx.moveTo(px, b.h - b.mb);
      ctx.lineTo(px, b.h - b.mb + 7);
      ctx.stroke();
      const halfLabel = ctx.measureText(label).width / 2;
      const labelX = clamp(px, b.ml + halfLabel + 2, b.w - b.mr - halfLabel - 2);
      ctx.textAlign = "center";
      if (!p.hideAxisText) ctx.fillText(label, labelX, b.h - b.mb + 18);
    });
    ctx.strokeStyle = "#9ca3af";
    ctx.lineWidth = 0.75;
    for (const ty of axisTicks(p, "y").minor) {
      const py = yScale(s, ty);
      if (py < b.mt || py > b.h - b.mb) continue;
      ctx.textAlign = "right";
      ctx.beginPath();
      ctx.moveTo(b.ml, py);
      ctx.lineTo(b.ml - 4, py);
      ctx.stroke();
    }
    ctx.strokeStyle = "#111827";
    ctx.lineWidth = 1.5;
    axisTicks(p, "y").major.forEach((ty, i) => {
      const py = yScale(s, ty);
      if (py < b.mt || py > b.h - b.mb) return;
      ctx.textAlign = "right";
      const label = axisTicks(p, "y").labels[i] ?? fmt(ty);
      ctx.beginPath();
      ctx.moveTo(b.ml, py);
      ctx.lineTo(b.ml - 7, py);
      ctx.stroke();
      if (!p.hideAxisText) ctx.fillText(label, b.ml - 9, clamp(py + 4, b.mt + 9, b.h - b.mb - 2));
    });
  }

  function drawOverlay(s) {
    const ctx = s.overlay.getContext("2d");
    ctx.clearRect(0, 0, s.box.w, s.box.h);
    s.labelBoxes = [];
    // Agent note: Gate selection is shared between panels; editing appearance belongs only to the active panel.
    const active = !!s.wrap.closest?.(".gate-history-plot.active");
    for (const g of s.payload.gates || []) drawGate(s, ctx, g, active && g.id === s.payload.selected);
    if (active && s.draft) drawGate(s, ctx, s.draft, true, true);
    const poly = s.payload.polygon || [];
    if (active && poly.length) drawPolyPoints(s, ctx, poly);
  }

  function drawGate(s, ctx, gate, selected, draft) {
    const g = gate.geometry || {};
    // Agent note: Inactive geometry must not cover axes/titles. Selected gates
    // retain their outside handles for editing; labels are positioned separately.
    const clipped = !selected && !draft;
    ctx.save();
    if (clipped) {
      ctx.beginPath();
      ctx.rect(s.box.ml, s.box.mt, s.box.w - s.box.ml - s.box.mr, s.box.h - s.box.mt - s.box.mb);
      ctx.clip();
    }
    ctx.strokeStyle = draft ? "#1d4ed8" : (gate.color || "#e56789");
    ctx.fillStyle = ctx.strokeStyle;
    ctx.lineWidth = selected ? 3.6 : 1.4;
    ctx.beginPath();
    if (gate.type === "rectangle" || gate.type === "range") {
      const x1 = xScale(s, g.xmin), x2 = xScale(s, g.xmax);
      const y1 = gate.type === "range" ? s.box.mt : yScale(s, g.ymax);
      const y2 = gate.type === "range" ? s.box.h - s.box.mb : yScale(s, g.ymin);
      ctx.rect(Math.min(x1, x2), Math.min(y1, y2), Math.abs(x2 - x1), Math.abs(y2 - y1));
    } else if (gate.type === "ellipse") {
      ctx.ellipse((xScale(s, g.xmin) + xScale(s, g.xmax)) / 2, (yScale(s, g.ymin) + yScale(s, g.ymax)) / 2,
        Math.abs(xScale(s, g.xmax) - xScale(s, g.xmin)) / 2, Math.abs(yScale(s, g.ymax) - yScale(s, g.ymin)) / 2, 0, 0, Math.PI * 2);
    } else if (gate.type === "polygon") {
      const vs = vertices(g);
      vs.forEach((v, i) => i ? ctx.lineTo(xScale(s, v.x), yScale(s, v.y)) : ctx.moveTo(xScale(s, v.x), yScale(s, v.y)));
      if (vs.length && (!draft || gate.closed)) ctx.closePath();
    } else if (gate.type === "quadrant" || gate.type === "curved_quadrant") {
      ctx.moveTo(xScale(s, g.x), s.box.mt); ctx.lineTo(xScale(s, g.x), s.box.h - s.box.mb);
      ctx.moveTo(s.box.ml, yScale(s, g.y)); ctx.lineTo(s.box.w - s.box.mr, yScale(s, g.y));
    }
    ctx.stroke();
    ctx.shadowBlur = 0;
    if (selected) handles(s, gate).forEach(h => square(ctx, h.px, h.py, 5));
    ctx.restore();
    if (!draft) drawGateLabel(s, ctx, gate);
  }

  function repelGateLabel(s, preferredX, preferredY, w, h) {
    // Agent note: Repel in display space only. Never move saved gate geometry or
    // label anchors; recompute collision boxes on every draw and export.
    const fit = (x, y) => ({x: clamp(x, s.box.ml + 2, Math.max(s.box.ml + 2, s.box.w - s.box.mr - w - 2)),
      y: clamp(y, s.box.mt + 2, Math.max(s.box.mt + 2, s.box.h - s.box.mb - h - 2))});
    const origin = fit(preferredX, preferredY);
    let best = origin, bestScore = Infinity;
    for (let radius = 0; radius <= 12; radius++) {
      const offsets = radius ? [[0,-radius],[0,radius],[-radius,0],[radius,0],[-radius,-radius],[radius,-radius],[-radius,radius],[radius,radius]] : [[0,0]];
      for (const [dx,dy] of offsets) {
        const candidate = fit(origin.x + dx * (w + 8) / 3, origin.y + dy * (h + 8));
        const overlap = (s.labelBoxes || []).reduce((sum, box) => sum +
          Math.max(0, Math.min(candidate.x + w + 4, box.x + box.w + 4) - Math.max(candidate.x - 4, box.x - 4)) *
          Math.max(0, Math.min(candidate.y + h + 4, box.y + box.h + 4) - Math.max(candidate.y - 4, box.y - 4)), 0);
        const score = overlap * 1e6 + Math.hypot(candidate.x - origin.x, candidate.y - origin.y);
        if (score < bestScore) { best = candidate; bestScore = score; }
      }
      if (bestScore < 1e6) break;
    }
    return best;
  }

  function drawGateLabel(s, ctx, gate) {
    const label = gate.label || {};
    const text = String(label.text || gate.name || "").trim();
    if (!text) return;
    const p = labelPoint(s, gate);
    const lines = text.split(/\n/).filter(Boolean).slice(0, 2);
    if (!lines.length) return;
    ctx.save();
    ctx.font = "12px system-ui, -apple-system, BlinkMacSystemFont, sans-serif";
    const padX = 6, padY = 4, lineH = 15;
    const w = Math.max(...lines.map(line => ctx.measureText(line).width)) + padX * 2;
    const h = lines.length * lineH + padY * 2;
    const placed = repelGateLabel(s, p.px, p.py, w, h);
    const x = placed.x, y = placed.y;
    gate._labelBox = { x, y, w, h };
    (s.labelBoxes || (s.labelBoxes = [])).push(gate._labelBox);
    ctx.fillStyle = "rgba(255,255,255,0.55)";
    ctx.strokeStyle = gate.color || "#e56789";
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.rect(x, y, w, h);
    ctx.fill();
    ctx.stroke();
    ctx.fillStyle = gate.color || "#374151";
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    lines.forEach((line, i) => ctx.fillText(line, x + padX, y + padY + i * lineH));
    ctx.restore();
  }

  function drawPolyPoints(s, ctx, pts) {
    ctx.strokeStyle = "#1d4ed8"; ctx.fillStyle = "#1d4ed8"; ctx.lineWidth = 2;
    ctx.beginPath();
    pts.forEach((v, i) => i ? ctx.lineTo(xScale(s, v.x), yScale(s, v.y)) : ctx.moveTo(xScale(s, v.x), yScale(s, v.y)));
    ctx.stroke();
    pts.forEach(v => square(ctx, xScale(s, v.x), yScale(s, v.y), 4));
    if (pts.length >= 3) ring(ctx, xScale(s, pts[0].x), yScale(s, pts[0].y), 7);
  }

  // Agent note: Export reuses the live drawing functions; SVG records vector marks rather than embedding a canvas bitmap.
  function svgContext(width, height) {
    const ns = "http://www.w3.org/2000/svg";
    const svg = document.createElementNS(ns, "svg");
    svg.setAttribute("xmlns", ns);
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
    svg.setAttribute("width", width); svg.setAttribute("height", height);
    const measure = document.createElement("canvas").getContext("2d");
    let matrix = new DOMMatrix(), path = [], stack = [], clipId = null, clipCount = 0;
    const ctx = { fillStyle: "black", strokeStyle: "black", globalAlpha: 1, lineWidth: 1,
      font: "12px sans-serif", textAlign: "left", textBaseline: "alphabetic" };
    function element(type, attrs, paint) {
      const el = document.createElementNS(ns, type);
      Object.entries(attrs).forEach(([key, value]) => el.setAttribute(key, value));
      el.setAttribute("transform", matrix.toString());
      el.setAttribute("opacity", ctx.globalAlpha);
      el.setAttribute("fill", paint === "fill" ? ctx.fillStyle : "none");
      if (paint === "stroke") { el.setAttribute("stroke", ctx.strokeStyle); el.setAttribute("stroke-width", ctx.lineWidth); }
      if (clipId) el.setAttribute("clip-path", `url(#${clipId})`);
      svg.appendChild(el); return el;
    }
    ctx.clearRect = () => {};
    ctx.setTransform = (a,b,c,d,e,f) => { matrix = new DOMMatrix([a,b,c,d,e,f]); };
    ctx.save = () => stack.push({matrix, clipId, fillStyle: ctx.fillStyle, strokeStyle: ctx.strokeStyle,
      globalAlpha: ctx.globalAlpha, lineWidth: ctx.lineWidth, font: ctx.font, textAlign: ctx.textAlign, textBaseline: ctx.textBaseline});
    ctx.restore = () => { const saved = stack.pop(); if (saved) { matrix = saved.matrix; clipId = saved.clipId; Object.assign(ctx, saved); } };
    ctx.translate = (x,y) => { matrix = matrix.translate(x,y); };
    ctx.rotate = angle => { matrix = matrix.rotate(angle * 180 / Math.PI); };
    ctx.beginPath = () => { path = []; };
    ctx.moveTo = (x,y) => path.push(`M ${x} ${y}`);
    ctx.lineTo = (x,y) => path.push(`L ${x} ${y}`);
    ctx.closePath = () => path.push("Z");
    ctx.rect = (x,y,w,h) => path.push(`M ${x} ${y} h ${w} v ${h} h ${-w} Z`);
    ctx.ellipse = (x,y,rx,ry) => path.push(`M ${x+rx} ${y} A ${rx} ${ry} 0 1 0 ${x-rx} ${y} A ${rx} ${ry} 0 1 0 ${x+rx} ${y} Z`);
    ctx.arc = (x,y,r) => ctx.ellipse(x,y,r,r);
    ctx.clip = () => {
      const defs = document.createElementNS(ns, "defs");
      const clip = document.createElementNS(ns, "clipPath");
      clipId = `gate-clip-${++clipCount}`;
      clip.setAttribute("id", clipId);
      clip.setAttribute("clipPathUnits", "userSpaceOnUse");
      const shape = document.createElementNS(ns, "path");
      shape.setAttribute("d", path.join(" "));
      shape.setAttribute("transform", matrix.toString());
      clip.appendChild(shape); defs.appendChild(clip); svg.appendChild(defs);
    };
    ctx.stroke = () => element("path", {d: path.join(" ")}, "stroke");
    ctx.fill = () => element("path", {d: path.join(" ")}, "fill");
    ctx.fillRect = (x,y,w,h) => element("rect", {x,y,width:w,height:h}, "fill");
    ctx.measureText = text => { measure.font = ctx.font; return measure.measureText(text); };
    ctx.fillText = (text,x,y) => {
      const el = element("text", {x,y, "text-anchor": ctx.textAlign === "center" ? "middle" : ctx.textAlign === "right" ? "end" : "start",
        "dominant-baseline": ctx.textBaseline === "top" ? "text-before-edge" : "alphabetic"}, "fill");
      el.style.font = ctx.font; el.textContent = text;
    };
    return {ctx, svg};
  }

  async function exportPlot(s, format, axisText) {
    if (!s || !s.payload || !s.box || s.payload.blank) return;
    const exported = Object.assign({}, s, {
      // Agent note: Export gates in black without mutating the interactive palette.
      payload: Object.assign({}, s.payload, {hideAxisText: !axisText, selected: null, polygon: [],
        gates: (s.payload.gates || []).map(gate => Object.assign({}, gate, {color: "#000000"}))}),
      draft: null
    });
    let blob;
    if (format === "svg") {
      const vector = svgContext(s.box.w, s.box.h);
      exported.points = exported.overlay = {getContext: () => vector.ctx};
      draw(exported);
      blob = new Blob([new XMLSerializer().serializeToString(vector.svg)], {type: "image/svg+xml"});
    } else {
      const canvas = document.createElement("canvas");
      const scale = 2048 / Math.max(s.box.w, s.box.h);
      canvas.width = Math.round(s.box.w * scale); canvas.height = Math.round(s.box.h * scale);
      const ctx = canvas.getContext("2d");
      ctx.setTransform(scale, 0, 0, scale, 0, 0);
      exported.points = {getContext: () => ctx};
      // Overlay clearing must not erase the exported point layer.
      const overlay = new Proxy(ctx, {get(target, key) {
        if (key === "clearRect") return () => {};
        const value = target[key]; return typeof value === "function" ? value.bind(target) : value;
      }, set(target, key, value) { target[key] = value; return true; }});
      exported.overlay = {getContext: () => overlay};
      draw(exported);
      blob = await new Promise(resolve => canvas.toBlob(resolve, "image/png"));
    }
    if (!blob) return;
    const link = document.createElement("a"), url = URL.createObjectURL(blob);
    const title = s.wrap.querySelector(".shiny-html-output");
    const name = ((title && title.textContent) || s.payload.id || "gating-plot").trim().replace(/[^a-zA-Z0-9_-]+/g, "_").slice(0, 120);
    link.href = url; link.download = `${name}.${format}`;
    link.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
  }
  document.addEventListener("click", function (event) {
    const button = event.target.closest("[data-gate-export]");
    if (!button) return;
    const wrap = button.closest(".gate-canvas-wrap");
    const preference = wrap.querySelector(".gate-export-axis-text");
    exportPlot(state.get(wrap.id), button.dataset.gateExport, !preference || preference.checked);
  });

  function bind(s) {
    s.overlay.addEventListener("pointerdown", e => pointerDown(s, e));
    s.overlay.addEventListener("pointermove", e => pointerMove(s, e));
    s.overlay.addEventListener("pointerleave", () => { if (!s.drag) s.overlay.style.cursor = ""; });
    window.addEventListener("pointerup", e => pointerUp(s, e));
    s.overlay.addEventListener("dblclick", e => {
      const hit = hitTest(s, eventPoint(s, e));
      if (hit && hit.gate && hit.gate.id && hit.kind === "body") {
        input(s.payload.input.drill, hit.gate.id);
        e.preventDefault();
        return;
      }
      if (closePolygon(s)) e.preventDefault();
    });
  }

  function pointerDown(s, e) {
    if (!s.payload) return;
    if (s.payload.blank) return;
    input(s.payload.input && s.payload.input.activate, { plot: s.payload.plot || "", nonce: Date.now() });
    if (s.payload.readOnly) return;
    const pt = eventPoint(s, e);
    const hit = hitTest(s, pt);
    if (hit) {
      s.drag = { hit, start: pt, original: clone(hit.gate.geometry), originalLabel: clone(hit.gate.label || {}) };
      s.overlay.style.cursor = "grabbing";
      if (hit.gate.id) input(s.payload.input.select, hit.gate.id);
      return;
    }
    if (s.payload.tool === "polygon" || s.payload.tool === "rectangle") {
      s.drag = { hit: { kind: "smart-polygon" }, start: pt, original: null, moved: false };
      return;
    }
    s.drag = { hit: { kind: "new", type: s.payload.tool }, start: pt, original: null };
  }

  function pointerMove(s, e) {
    const pt = eventPoint(s, e);
    if (!s.drag || (s.payload && s.payload.readOnly)) {
      updateCursor(s, pt);
      return;
    }
    if (s.drag.hit.kind === "label") {
      s.drag.hit.gate.label = moveLabel(s, s.drag, pt);
      drawOverlay(s);
      return;
    }
    const d = makeDragGeometry(s, s.drag, pt);
    if (!d) return;
    if (s.drag.hit.kind === "smart-polygon") {
      if (Math.hypot(pt.x - s.drag.start.x, pt.y - s.drag.start.y) < 6) return;
      s.drag.moved = true;
      s.draft = { type: "rectangle", geometry: d, color: "#1d4ed8" };
      input(s.payload.input.draft, draftPayload(s, { type: "rectangle", geometry: d }));
    } else if (s.drag.hit.kind === "new") {
      s.draft = { type: s.drag.hit.type, geometry: d, color: "#1d4ed8" };
      input(s.payload.input.draft, draftPayload(s, { type: s.drag.hit.type, geometry: d }));
    } else {
      s.drag.hit.gate.geometry = d;
    }
    drawOverlay(s);
    s.overlay.style.cursor = "grabbing";
  }

  function pointerUp(s) {
    if (!s.drag) return;
    if (s.payload && s.payload.readOnly) {
      s.drag = null;
      updateCursor(s);
      return;
    }
    const h = s.drag.hit;
    if (h.kind === "smart-polygon") {
      if (s.drag.moved && s.draft && s.draft.type === "rectangle") {
        input(s.payload.input.draft, draftPayload(s, { type: "rectangle", geometry: s.draft.geometry, closed: true, nonce: Date.now() }));
        s.drag = null;
        updateCursor(s);
        return;
      }
      if (!s.drag.moved) addPolygonPoint(s, s.drag.start);
      s.drag = null;
      updateCursor(s);
      return;
    }
    if (h.kind === "label" && h.gate && h.gate.id) {
      input(s.payload.input.label, { id: h.gate.id, label_pos: h.gate.label });
    } else if (h.kind !== "new" && h.gate && h.gate.id) {
      input(s.payload.input.geometry, { id: h.gate.id, geometry: h.gate.geometry });
    }
    s.drag = null;
    updateCursor(s);
  }

  function updateCursor(s, pt) {
    if (!s.overlay || !s.payload || s.payload.blank) {
      if (s.overlay) s.overlay.style.cursor = "";
      return;
    }
    if (s.drag) {
      s.overlay.style.cursor = "grabbing";
      return;
    }
    if (s.payload.readOnly) {
      s.overlay.style.cursor = "pointer";
      return;
    }
    const hit = hitTest(s, pt || {
      x: -Infinity,
      y: -Infinity
    });
    s.overlay.style.cursor = hit ? "grab" : "crosshair";
  }

  function makeDragGeometry(s, drag, pt) {
    const h = drag.hit, o = clone(drag.original || {});
    const sx = xData(s, drag.start.x), sy = yData(s, drag.start.y);
    const x = xData(s, pt.x), y = yData(s, pt.y);
    const dx = x - sx, dy = y - sy;
    if (h.kind === "new") return newGeom(h.type, sx, sy, x, y);
    if (h.kind === "smart-polygon") return newGeom("rectangle", sx, sy, x, y);
    if (h.kind === "body") return moveGeom(h.gate.type, o, dx, dy);
    if (h.kind === "vertex") {
      const vs = vertices(o);
      vs[h.index] = { x, y };
      return { vertices: vs };
    }
    if (h.kind === "handle") return resizeGeom(h.gate.type, o, h.handle, x, y);
    return null;
  }

  function newGeom(type, sx, sy, x, y) {
    if (type === "quadrant" || type === "curved_quadrant") return { x, y, quadrant: "upper_right", curve: type === "curved_quadrant" ? 0.25 : 0 };
    return { xmin: Math.min(sx, x), xmax: Math.max(sx, x), ymin: Math.min(sy, y), ymax: Math.max(sy, y) };
  }

  function hitTest(s, pt) {
    const gates = (s.payload.gates || []).slice().reverse();
    for (const g of gates) {
      const b = g._labelBox;
      if (b && pt.x >= b.x && pt.x <= b.x + b.w && pt.y >= b.y && pt.y <= b.y + b.h) return { kind: "label", gate: g };
    }
    for (const g of gates) {
      for (const h of handles(s, g)) if (Math.hypot(h.px - pt.x, h.py - pt.y) < 9) return { kind: h.kind, handle: h.handle, index: h.index, gate: g };
      if (insideGate(s, g, pt)) return { kind: "body", gate: g };
    }
    return null;
  }

  function closePolygon(s) {
    if (!s.payload || !s.draft || s.draft.type !== "polygon") return false;
    const pts = s.draft.geometry.vertices || [];
    if (pts.length < 3) return false;
    const geometry = { vertices: pts };
    input(s.payload.input.polygon, pts);
    s.payload.polygon = pts.slice();
    s.draft = { type: "polygon", geometry, color: "#1d4ed8", closed: true };
    input(s.payload.input.draft, draftPayload(s, { type: "polygon", geometry, closed: true, nonce: Date.now() }));
    drawOverlay(s);
    return true;
  }

  function addPolygonPoint(s, pt) {
    let pts = s.draft && s.draft.type === "polygon" ? s.draft.geometry.vertices.slice() : [];
    if (pts.length >= 3 && distPx(s, pts[0], pt) < 10) {
      closePolygon(s);
      return;
    }
    pts.push({ x: xData(s, pt.x), y: yData(s, pt.y) });
    s.draft = { type: "polygon", geometry: { vertices: pts }, color: "#1d4ed8" };
    input(s.payload.input.draft, draftPayload(s, { type: "polygon", geometry: { vertices: pts } }));
    drawOverlay(s);
  }

  function handles(s, gate) {
    const g = gate.geometry || {};
    if (gate.type === "polygon") return vertices(g).map((v, i) => ({ kind: "vertex", index: i, px: xScale(s, v.x), py: yScale(s, v.y) }));
    if (gate.type === "quadrant" || gate.type === "curved_quadrant") return [{ kind: "handle", handle: "center", px: xScale(s, g.x), py: yScale(s, g.y) }];
    return [
      { kind: "handle", handle: "minmin", px: xScale(s, g.xmin), py: yScale(s, g.ymin) },
      { kind: "handle", handle: "minmax", px: xScale(s, g.xmin), py: yScale(s, g.ymax) },
      { kind: "handle", handle: "maxmin", px: xScale(s, g.xmax), py: yScale(s, g.ymin) },
      { kind: "handle", handle: "maxmax", px: xScale(s, g.xmax), py: yScale(s, g.ymax) }
    ];
  }

  function labelPoint(s, gate) {
    const label = gate.label || {};
    if (finite(label.x) && finite(label.y)) return { px: xScale(s, label.x), py: yScale(s, label.y) };
    const g = gate.geometry || {};
    if (gate.type === "polygon") {
      const vs = vertices(g).filter(v => finite(v.x) && finite(v.y));
      if (vs.length) return { px: xScale(s, vs.reduce((a, v) => a + v.x, 0) / vs.length), py: yScale(s, vs.reduce((a, v) => a + v.y, 0) / vs.length) };
    }
    if (gate.type === "quadrant" || gate.type === "curved_quadrant") return { px: xScale(s, g.x) + 10, py: yScale(s, g.y) - 26 };
    return { px: xScale(s, g.xmin), py: yScale(s, g.ymax) - 26 };
  }

  function moveLabel(s, drag, pt) {
    const o = drag.originalLabel || {};
    const sx = xData(s, drag.start.x), sy = yData(s, drag.start.y);
    const dx = xData(s, pt.x) - sx, dy = yData(s, pt.y) - sy;
    return Object.assign({}, o, { x: Number(o.x || 0) + dx, y: Number(o.y || 0) + dy });
  }

  function insideGate(s, gate, pt) {
    const x = xData(s, pt.x), y = yData(s, pt.y), g = gate.geometry || {};
    if (gate.type === "rectangle" || gate.type === "range") return x >= g.xmin && x <= g.xmax && (gate.type === "range" || (y >= g.ymin && y <= g.ymax));
    if (gate.type === "ellipse") {
      const cx = (g.xmin + g.xmax) / 2, cy = (g.ymin + g.ymax) / 2, rx = Math.abs(g.xmax - g.xmin) / 2, ry = Math.abs(g.ymax - g.ymin) / 2;
      return rx > 0 && ry > 0 && ((x - cx) / rx) ** 2 + ((y - cy) / ry) ** 2 <= 1;
    }
    if (gate.type === "polygon") return pointInPolygon(x, y, vertices(g));
    if (gate.type === "quadrant" || gate.type === "curved_quadrant") return Math.abs(xScale(s, g.x) - pt.x) < 8 || Math.abs(yScale(s, g.y) - pt.y) < 8;
    return false;
  }

  function moveGeom(type, g, dx, dy) {
    if (type === "polygon") return { vertices: vertices(g).map(v => ({ x: v.x + dx, y: v.y + dy })) };
    if (type === "quadrant" || type === "curved_quadrant") return Object.assign({}, g, { x: g.x + dx, y: g.y + dy });
    return { xmin: g.xmin + dx, xmax: g.xmax + dx, ymin: g.ymin + dy, ymax: g.ymax + dy };
  }

  function resizeGeom(type, g, handle, x, y) {
    if (type === "quadrant" || type === "curved_quadrant") return Object.assign({}, g, { x, y });
    const out = clone(g);
    if (handle.includes("min")) out.xmin = x;
    if (handle.includes("max")) out.xmax = x;
    if (handle === "minmin" || handle === "maxmin") out.ymin = y;
    if (handle === "minmax" || handle === "maxmax") out.ymax = y;
    if (out.xmin > out.xmax) [out.xmin, out.xmax] = [out.xmax, out.xmin];
    if (out.ymin > out.ymax) [out.ymin, out.ymax] = [out.ymax, out.ymin];
    return out;
  }

  function vertices(g) {
    return asPoints(g.vertices || {});
  }
  function asPoints(vs) {
    if (Array.isArray(vs)) return vs.map(v => ({ x: Number(v.x), y: Number(v.y) }));
    if (Array.isArray(vs.x) && Array.isArray(vs.y)) return vs.x.map((x, i) => ({ x: Number(x), y: Number(vs.y[i]) }));
    return Object.keys(vs).map(k => vs[k]).map(v => ({ x: Number(v.x), y: Number(v.y) }));
  }
  function eventPoint(s, e) {
    const r = s.overlay.getBoundingClientRect();
    return {
      x: (e.clientX - r.left) * (s.overlay.offsetWidth || r.width) / (r.width || 1),
      y: (e.clientY - r.top) * (s.overlay.offsetHeight || r.height) / (r.height || 1)
    };
  }
  function distPx(s, v, pt) { return Math.hypot(xScale(s, v.x) - pt.x, yScale(s, v.y) - pt.y); }
  function clone(o) { return JSON.parse(JSON.stringify(o || {})); }
  function square(ctx, x, y, r) { ctx.fillRect(x - r, y - r, r * 2, r * 2); }
  function ring(ctx, x, y, r) {
    ctx.save();
    ctx.strokeStyle = "#1d4ed8";
    ctx.fillStyle = "#ffffff";
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.arc(x, y, r, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();
    ctx.restore();
  }
  function ticks(lo, hi) {
    lo = Number(lo); hi = Number(hi);
    if (!Number.isFinite(lo) || !Number.isFinite(hi) || lo === hi) return [];
    const span = Math.abs(hi - lo);
    const raw = span / 4;
    const pow = Math.pow(10, Math.floor(Math.log10(raw)));
    const m = raw / pow;
    const step = (m <= 1 ? 1 : m <= 2 ? 2 : m <= 5 ? 5 : 10) * pow;
    const out = [];
    for (let v = Math.ceil(lo / step) * step; v <= hi + step * 1e-9; v += step) out.push(Number(v.toPrecision(12)));
    return out;
  }
  function axisTitle(p, axis) {
    const title = p.labels[axis] || "";
    const multiplier = p.ticks && p.ticks[axis] && p.ticks[axis].multiplier;
    return multiplier ? title + " (" + multiplier + ")" : title;
  }
  function axisTicks(p, axis) {
    const t = p.ticks && p.ticks[axis];
    if (t && Array.isArray(t.major)) {
      const major = t.major.map((value, i) => ({ value: Number(value), label: Array.isArray(t.labels) ? t.labels[i] : null }))
        .filter(item => Number.isFinite(item.value));
      return {
        major: major.map(item => item.value),
        minor: Array.isArray(t.minor) ? t.minor.map(Number).filter(Number.isFinite) : [],
        labels: major.map(item => item.label)
      };
    }
    const r = p.ranges[axis];
    const major = ticks(r[0], r[1]);
    return { major, minor: [], labels: major.map(fmt) };
  }
  function fmt(v) {
    v = Number(v);
    if (!Number.isFinite(v)) return "";
    const av = Math.abs(v);
    const exponent = Math.round(Math.log10(av));
    if (av >= 10000 && Math.abs(av - Math.pow(10, exponent)) <= av * 1e-8) return sciLabel(v);
    if (av >= 1000) return v.toLocaleString("en-US", {maximumFractionDigits: 8});
    const rounded = Math.round(v * 100) / 100;
    return String(rounded).replace(/\.0+$/, "").replace(/(\.\d*[1-9])0+$/, "$1");
  }
  function sciLabel(v) {
    const supers = { "-": "⁻", "0": "⁰", "1": "¹", "2": "²", "3": "³", "4": "⁴", "5": "⁵", "6": "⁶", "7": "⁷", "8": "⁸", "9": "⁹" };
    const av = Math.abs(v);
    const exp = Math.floor(Math.log10(av));
    const coef = v / Math.pow(10, exp);
    const coefText = Math.abs(coef - 1) < 0.005 ? "" : (Math.round(coef * 100) / 100).toString().replace(/\.0+$/, "") + "×";
    return coefText + "10" + String(exp).split("").map(ch => supers[ch] || ch).join("");
  }
  function ramp(t) {
    t = clamp(t, 0, 1);
    const stops = [
      [0.00, [0, 0, 255]],
      [0.25, [0, 255, 255]],
      [0.50, [0, 255, 0]],
      [0.75, [255, 255, 0]],
      [1.00, [255, 0, 0]]
    ];
    for (let i = 1; i < stops.length; i++) {
      if (t > stops[i][0]) continue;
      const a = stops[i - 1], b = stops[i];
      const u = (t - a[0]) / (b[0] - a[0] || 1);
      const c = a[1].map((v, j) => Math.round(v + (b[1][j] - v) * u));
      return `rgb(${c[0]},${c[1]},${c[2]})`;
    }
    return "rgb(255,0,0)";
  }
  function groupColor(p, g) {
    if (!g || g === "Ungated") return "#b8bec7";
    if (p.groupColors && p.groupColors[g]) return p.groupColors[g];
    let h = 0; for (let i = 0; i < g.length; i++) h = (h * 31 + g.charCodeAt(i)) >>> 0;
    return palette[h % palette.length];
  }
  function pointInPolygon(x, y, vs) {
    let inside = false;
    for (let i = 0, j = vs.length - 1; i < vs.length; j = i++) {
      const xi = vs[i].x, yi = vs[i].y, xj = vs[j].x, yj = vs[j].y;
      const hit = ((yi > y) !== (yj > y)) && x < (xj - xi) * (y - yi) / ((yj - yi) || 1e-12) + xi;
      if (hit) inside = !inside;
    }
    return inside;
  }

  function clearDraft(msg) {
    const s = state.get(msg.id);
    if (!s) return;
    s.draft = null;
    if (s.payload) s.payload.polygon = [];
    draw(s);
  }
  function updateToolboxPills() {
    document.querySelectorAll(".plot-toolbox:not(.gate-toolbar), .Pinhki-plot-toolbox:not(.gate-toolbar)").forEach(box => {
      const checked = box.querySelector("input[type='radio']:checked");
      const item = checked && (checked.closest(".radio-inline") || checked.closest(".radio") || checked.closest("label"));
      if (!item) return;
      const b = box.getBoundingClientRect();
      const r = item.getBoundingClientRect();
      box.style.setProperty("--app-tab-pill-x", (r.left - b.left) + "px");
      box.style.setProperty("--app-tab-pill-w", r.width + "px");
    });
  }
  function scheduleToolboxPills() {
    requestAnimationFrame(() => requestAnimationFrame(() => {
      updateToolboxPills();
    }));
  }
  let registered = false;
  function register() {
    if (registered || !window.Shiny) return;
    Shiny.addCustomMessageHandler("flowjo-gating-payload", setup);
    Shiny.addCustomMessageHandler("flowjo-gating-clear-draft", clearDraft);
    Shiny.addCustomMessageHandler("flowjo-gating-select", msg => {
      for (const s of state.values()) {
        if (!s.payload) continue;
        s.payload.selected = (s.payload.gates || []).some(g => g.id === msg.selected) ? msg.selected : null;
        drawOverlay(s);
      }
    });
    Shiny.addCustomMessageHandler("flowjo-gating-active-plot", msg => {
      const root = document.getElementById(msg.id);
      if (!root) return;
      root.querySelectorAll(".gate-history-plot").forEach(el => {
        el.classList.toggle("active", el.dataset.plot === msg.active);
      });
      for (const s of state.values()) {
        if (s.payload && root.contains(s.wrap)) drawOverlay(s);
      }
      scheduleCanvasRemeasure();
    });
    registered = true;
    // Agent note: Only real server work starts a loader; UI echoes can arrive after draw.
    Shiny.addCustomMessageHandler("flowjo-gating-pending", msg => {
      document.querySelectorAll(".gate-canvas-wrap").forEach(wrap => {
        if (msg.pending) queuePending(wrap);
        else clearPending(wrap);
      });
    });
    scheduleToolboxPills();
  }
  document.addEventListener("change", e => {
    if (e.target && e.target.matches(".plot-toolbox input[type='radio'], .Pinhki-plot-toolbox input[type='radio']")) scheduleToolboxPills();
  });
  document.addEventListener("click", e => {
    if (e.target && e.target.closest(".plot-toolbox, .Pinhki-plot-toolbox")) scheduleToolboxPills();
  });
  document.addEventListener("keydown", e => {
    const card = e.target && e.target.closest && e.target.closest(".gate-preview-card");
    if (!card) return;
    if (e.target.closest("input, textarea, select, .selectize-input, .modal, table.dataTable")) return;
    const map = { ArrowLeft: "left", ArrowRight: "right", ArrowUp: "up", ArrowDown: "down" };
    const dir = map[e.key];
    if (!dir) return;
    input(card.id.replace(/preview_card$/, "preview_key_nav"), { dir, nonce: Date.now() });
    e.preventDefault();
  });
  window.addEventListener("resize", () => {
    remeasureAllCanvases();
    scheduleToolboxPills();
  });
  document.addEventListener("shiny:connected", register);
  document.addEventListener("shiny:value", scheduleToolboxPills);
  register();
})();
