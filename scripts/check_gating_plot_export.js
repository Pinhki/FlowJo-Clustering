const assert = require("assert");
const fs = require("fs"), vm = require("vm");
class Element {
  constructor(tag) { this.tag = tag; this.attrs = {}; this.style = {}; this.children = []; this.textContent = ""; }
  setAttribute(k,v) { this.attrs[k] = String(v); }
  appendChild(child) { this.children.push(child); }
  click() {}
}
class Matrix {
  constructor(value) { this.value = value || [1,0,0,1,0,0]; }
  translate(x,y) { return new Matrix([1,0,0,1,x,y]); }
  rotate() { return this; }
  toString() { return `matrix(${this.value.join(",")})`; }
}
const blobs = [], downloads = [], canvases = [];
function context() {
  return {measureText: t => ({width: String(t).length * 7}), setTransform() {}, clearRect() {},
    fillRect() {}, clip() {}, beginPath() {}, moveTo() {}, lineTo() {}, stroke() {}, fill() {},
    rect() {}, ellipse() {}, arc() {}, closePath() {}, save() {}, restore() {}, translate() {}, rotate() {}, fillText() {}};
}
const document = {
  addEventListener() {}, querySelectorAll: () => [],
  createElementNS: (_,tag) => new Element(tag),
  createElement(tag) {
    const el = new Element(tag);
    if (tag === "canvas") { const ctx = context(); el.getContext = () => ctx;
      el.toBlob = fn => fn(new Blob(["png"],{type:"image/png"})); canvases.push(el); }
    if (tag === "a") el.click = () => downloads.push(el.download);
    return el;
  }
};
const serialize = el => `<${el.tag} ${Object.entries(el.attrs).map(([k,v])=>`${k}="${v}"`).join(" ")}>${el.textContent}${el.children.map(serialize).join("")}</${el.tag}>`;
const sandbox = {document, window: {addEventListener() {}}, DOMMatrix: Matrix, Blob,
  XMLSerializer: class {serializeToString(el) { return serialize(el); }},
  URL: {createObjectURL(blob) {blobs.push(blob); return "blob:test";}, revokeObjectURL() {}},
  setTimeout() {}, requestAnimationFrame() {}};
let src = fs.readFileSync("Contents/www/gating_canvas.js","utf8");
src = src.replace(/\}\)\(\);\s*$/, "window.plotCheck = {drawBins, exportPlot, axisTicks, repelGateLabel, drawOverlay}; })();");
vm.runInNewContext(src,sandbox);
const api = sandbox.window.plotCheck;
const payload = {id:"gating-test",mode:"pseudocolor",x:[.1,.1,.7],y:[.2,.2,.8],
  ranges:{x:[0,1],y:[0,1]},labels:{x:"FSC-A",y:"SSC-A"},ticks:{x:{major:[0,1],labels:["0","1"],minor:[.5]},y:{major:[0,1],labels:["0","1"],minor:[.5]}},
  gates:[{id:"gate",type:"rectangle",name:"Test gate",color:"#123456",geometry:{xmin:.2,xmax:.6,ymin:.2,ymax:.6},label:{text:"Test gate"}}]};
function state(w) {return {payload,box:{w,h:w,ml:58,mr:18,mt:58,mb:46},wrap:{querySelector:()=>({textContent:"Sample Root"})},points:{getContext:()=>context()},overlay:{getContext:()=>context()}};}
function density(w) {
  const recorded = [], ctx = {globalAlpha:1,fillRect(...args) {recorded.push({color:this.fillStyle,args});}};
  const s = state(w); s.points = {getContext:()=>ctx}; api.drawBins(s,"pseudocolor"); return recorded;
}
(async () => {
  const selectionState = state(600);
  selectionState.payload = {...payload, selected: "gate"};
  let active = false, clips = 0;
  const widths = [], overlayContext = context();
  overlayContext.clip = () => clips++;
  overlayContext.stroke = function() { widths.push(this.lineWidth); };
  selectionState.overlay.getContext = () => overlayContext;
  selectionState.wrap.closest = () => active ? {} : null;
  api.drawOverlay(selectionState);
  assert.strictEqual(clips, 1, "inactive selected gate must stay clipped");
  assert(!widths.includes(3.6), "inactive plot must not use editing stroke");
  active = true; clips = 0; widths.length = 0;
  api.drawOverlay(selectionState);
  assert.strictEqual(clips, 0, "active gate retains out-of-bounds editing handles");
  assert(widths.includes(3.6), "active plot retains editing stroke");
  const labelState = state(600); labelState.labelBoxes = [{x:100,y:100,w:120,h:38}];
  const label = api.repelGateLabel(labelState,100,100,120,38);
  assert(label.y + 38 <= 100 || label.y >= 138 || label.x + 120 <= 100 || label.x >= 220);
  const narrow = api.axisTicks({ticks:{y:{major:[],labels:[],minor:[1.2,1.5,1.8]}},ranges:{y:[1,2]}}, "y");
  assert.equal(narrow.major.length, 0);
  assert.deepEqual(Array.from(narrow.minor), [1.2,1.5,1.8]);
  assert.deepEqual(density(300).map(x=>x.color),density(1200).map(x=>x.color));
  await api.exportPlot(state(600),"svg",true);
  const withText = await blobs.pop().text();
  assert(withText.includes("<rect") && withText.includes("<path") && withText.includes("FSC-A") && withText.includes("Test gate"));
  assert(!withText.includes("<image"));
  assert(withText.includes("<clipPath") && withText.includes('clip-path="url(#gate-clip-'));
  assert(withText.includes('stroke="#000000"') && !withText.includes("#123456"));
  assert.equal(payload.gates[0].color, "#123456");
  await api.exportPlot(state(600),"svg",false);
  const withoutText = await blobs.pop().text();
  assert(!withoutText.includes("FSC-A") && !withoutText.includes("SSC-A") && withoutText.includes("Test gate"));
  await api.exportPlot(state(600),"png",false);
  assert.equal(canvases[canvases.length-1].width,2048);
  assert(downloads.some(x=>x.endsWith(".svg")) && downloads.some(x=>x.endsWith(".png")));
  console.log("PASS: density colors independent of panel size; SVG contains vector marks/gates and optional axis text; PNG renders at 2048 pixels. Canvas encoding and browser appearance require live verification.");
})().catch(error=>{console.error(error);process.exitCode=1;});
