#!/usr/bin/env node
// Compare two tasty-bench CSV files, plotting change in time and allocation
// per benchmark on the same SVG chart.
//
// Usage: node benchmark-alloc.js <old.csv> <new.csv>

const fs = require('fs');

const [oldPath, newPath] = process.argv.slice(2);
if (!oldPath || !newPath) {
  console.error('Usage: node benchmark-alloc.js <old.csv> <new.csv>');
  process.exit(1);
}

// Derive short labels from filenames, e.g. "benchmark-3.10.2.csv" -> "3.10.2"
const shortName = p =>
      p.replace(/^.*\//, '').replace(/^(benchmark-|bench_)/, '').replace(/\.csv$/, '');
const oldLabel = shortName(oldPath);
const newLabel = shortName(newPath);

function parseCsv(path) {
  const lines = fs.readFileSync(path, 'utf8').trim().split('\n');
  const out = new Map();
  for (const line of lines.slice(1)) {
    const cols = line.split(',');
    out.set(cols[0], { time: Number(cols[1]), alloc: Number(cols[3]) });
  }
  return out;
}

const oldData = parseCsv(oldPath);
const newData = parseCsv(newPath);

function fmtMs(ps) {
  const ms = ps / 1e9;
  if (ms >= 100) return ms.toFixed(0) + ' ms';
  if (ms >= 10) return ms.toFixed(1) + ' ms';
  return ms.toFixed(2) + ' ms';
}

function fmtBytes(b) {
  const mb = b / (1024 * 1024);
  if (mb >= 100) return mb.toFixed(0) + ' MB';
  if (mb >= 10) return mb.toFixed(1) + ' MB';
  if (mb >= 1) return mb.toFixed(2) + ' MB';
  return (b / 1024).toFixed(0) + ' KB';
}

// Collect rows per group
const groups = { writers: [], readers: [] };
for (const [name, o] of oldData) {
  const m = name.match(/^All\.(writers|readers)\.(.+)$/);
  if (!m) continue;
  const n = newData.get(name);
  if (!n) continue;
  groups[m[1]].push({
    label: m[2],
    oldTime: o.time, newTime: n.time,
    oldAlloc: o.alloc, newAlloc: n.alloc,
    timeChange: (n.time - o.time) / o.time,
    allocChange: (n.alloc - o.alloc) / o.alloc,
  });
}
// New benchmarks with no baseline
const added = [];
for (const [name] of newData) {
  if (!oldData.has(name)) added.push(name.replace(/^All\./, ''));
}

for (const g of Object.values(groups)) g.sort((a, b) => a.timeChange - b.timeChange);

// Colors: time = green, alloc = red (as requested)
const TIME_COLOR = '#2f9e44';
const ALLOC_COLOR = '#d9480f';

// Layout
const rowH = 24;
const labelW = 175;
const chartW = 430;
const rightW = 260;
const margin = 22;
const width = margin + labelW + chartW + rightW + margin;
const headerH = 100;
const sectionGap = 40;
const footerH = added.length ? 46 : 24;

const allRows = [...groups.writers, ...groups.readers];
const allChanges = allRows.flatMap(r => [r.timeChange, r.allocChange]);
const minC = Math.min(...allChanges);
const maxC = Math.max(...allChanges);
// domain with padding, always include 0
const lo = Math.min(0, minC) * 1.06;
const hi = Math.max(0.05, maxC * 1.4);
const x = c => margin + labelW + ((c - lo) / (hi - lo)) * chartW;

const sections = [
  ['Writers', groups.writers],
  ['Readers', groups.readers],
];

let height = headerH;
for (const [, rows] of sections) height += 34 + rows.length * rowH + sectionGap;
height += footerH - sectionGap;

const esc = s => s.replace(/&/g, '&amp;').replace(/</g, '&lt;');
let svg = [];
svg.push(`<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}" font-family="Helvetica, Arial, sans-serif">`);
svg.push(`<rect width="${width}" height="${height}" fill="#fcfcf9"/>`);
svg.push(`<text x="${margin}" y="34" font-size="21" font-weight="bold" fill="#222">pandoc benchmarks: ${esc(oldLabel)} \u2192 ${esc(newLabel)}</text>`);
svg.push(`<text x="${margin}" y="56" font-size="12.5" fill="#666">Change in mean wall-clock time and allocated bytes per benchmark (tasty-bench). Negative = better. Sorted by time.</text>`);
// Legend
const legY = 76;
svg.push(`<rect x="${margin}" y="${legY - 9}" width="16" height="9" fill="${TIME_COLOR}" rx="1.5"/>`);
svg.push(`<text x="${margin + 22}" y="${legY}" font-size="11.5" fill="#444">time</text>`);
svg.push(`<rect x="${margin + 70}" y="${legY - 9}" width="16" height="9" fill="${ALLOC_COLOR}" rx="1.5"/>`);
svg.push(`<text x="${margin + 92}" y="${legY}" font-size="11.5" fill="#444">allocation</text>`);

let y = headerH;
for (const [title, rows] of sections) {
  svg.push(`<text x="${margin}" y="${y + 14}" font-size="15" font-weight="bold" fill="#333">${title}</text>`);
  y += 24;
  const top = y, bottom = y + rows.length * rowH;

  // gridlines + axis ticks
  const step = 0.25;
  for (let t = Math.ceil(lo / step) * step; t <= hi + 1e-9; t += step) {
    const tx = x(t);
    const zero = Math.abs(t) < 1e-9;
    svg.push(`<line x1="${tx.toFixed(1)}" y1="${top}" x2="${tx.toFixed(1)}" y2="${bottom}" stroke="${zero ? '#888' : '#e4e4de'}" stroke-width="${zero ? 1.2 : 1}"/>`);
    svg.push(`<text x="${tx.toFixed(1)}" y="${bottom + 14}" font-size="10" fill="#888" text-anchor="middle">${(t * 100).toFixed(0)}%</text>`);
  }

  for (const r of rows) {
    const barH = 8, barGap = 2;
    const x0 = x(0);

    // one bar per metric: time first (top, green), then alloc (bottom, red)
    const bars = [
      { change: r.timeChange, color: TIME_COLOR, cy: y + rowH / 2 - (barH + barGap) / 2 },
      { change: r.allocChange, color: ALLOC_COLOR, cy: y + rowH / 2 + (barH + barGap) / 2 },
    ];
    for (const b of bars) {
      const x1 = x(b.change);
      const neg = b.change < 0;
      svg.push(`<rect x="${Math.min(x0, x1).toFixed(1)}" y="${(b.cy - barH / 2).toFixed(1)}" width="${Math.max(Math.abs(x1 - x0), 0.75).toFixed(1)}" height="${barH}" fill="${b.color}" rx="1.5"/>`);
      const pct = (b.change * 100).toFixed(Math.abs(b.change) < 0.095 ? 1 : 0) + '%';
      const pctTxt = (b.change > 0 ? '+' : '') + pct;
      const tipX = neg ? x1 - 4 : x1 + 4;
      svg.push(`<text x="${tipX.toFixed(1)}" y="${(b.cy + 3).toFixed(1)}" font-size="9" fill="${b.color}" text-anchor="${neg ? 'end' : 'start'}" font-weight="bold">${pctTxt}</text>`);
    }

    // benchmark label
    svg.push(`<text x="${margin + labelW - 8}" y="${(y + rowH / 2 + 3.5).toFixed(1)}" font-size="11" fill="#333" text-anchor="end">${esc(r.label)}</text>`);
    // absolute values on right: time on top line, alloc below
    const rx = margin + labelW + chartW + 14;
    svg.push(`<text x="${rx}" y="${(bars[0].cy + 3).toFixed(1)}" font-size="9" fill="#777">${fmtMs(r.oldTime)} \u2192 ${fmtMs(r.newTime)}</text>`);
    svg.push(`<text x="${rx}" y="${(bars[1].cy + 3).toFixed(1)}" font-size="9" fill="#777">${fmtBytes(r.oldAlloc)} \u2192 ${fmtBytes(r.newAlloc)}</text>`);
    y += rowH;
  }
  y += sectionGap;
}

if (added.length) {
  svg.push(`<text x="${margin}" y="${height - 26}" font-size="11" fill="#888">New benchmarks (no ${esc(oldLabel)} baseline): ${esc(added.join(', '))}</text>`);
}
svg.push(`<text x="${margin}" y="${height - 10}" font-size="10" fill="#aaa">Mean times and allocated bytes from tasty-bench CSV output; % change = (${esc(newLabel)} \u2212 ${esc(oldLabel)}) / ${esc(oldLabel)}.</text>`);
svg.push('</svg>');

const outFile = 'benchmark-comparison.svg';
fs.writeFileSync(outFile, svg.join('\n'));
console.log(`Wrote ${outFile} (${width}x${height})`);
console.log(`writers: ${groups.writers.length} rows, readers: ${groups.readers.length} rows`);
console.log(`time change range: ${(Math.min(...allRows.map(r => r.timeChange)) * 100).toFixed(1)}% .. ${(Math.max(...allRows.map(r => r.timeChange)) * 100).toFixed(1)}%`);
console.log(`alloc change range: ${(Math.min(...allRows.map(r => r.allocChange)) * 100).toFixed(1)}% .. ${(Math.max(...allRows.map(r => r.allocChange)) * 100).toFixed(1)}%`);
