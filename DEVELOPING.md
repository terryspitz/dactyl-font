# Developing dactyl-font

## Prerequisites

- [.NET 8 SDK](https://dotnet.microsoft.com/download)
- [Node.js 22+](https://nodejs.org/)
- [Fable](https://fable.io/) (installed via `dotnet tool restore` below)

## First-time setup

```bash
# 1. Clone with submodules (fmin is a git submodule)
git clone --recurse-submodules https://github.com/terryspitz/dactyl-font

# If you already cloned without --recurse-submodules, or created a worktree:
git submodule update --init --recursive

# 2. Restore .NET tools and dependencies
dotnet restore
dotnet tool restore

# 3. Install Node dependencies
cd web && npm ci && cd ..
```

## Running the dev server

This compiles F# via Fable (watch mode) and starts the Vite dev server concurrently:

```bash
npm run dev
```

The app will be available at `http://localhost:5173/dactyl-font/`.

## Screenshot tests

Tests use [Playwright](https://playwright.dev/) to screenshot each tab and
each tween axis, comparing against saved baseline PNGs.

```bash
# Build first (tests run against the production build via vite preview)
dotnet fable src/explorer --outDir web/src/lib/fable
cd web
npm ci
npx playwright install chromium
npm run build

# Run tab screenshot tests (compare against baselines)
npm run test:tabs

# Run tween screenshot tests (compare against saved baseline PNGs)
npm run test:tweens

# Regenerate tab baselines after an intentional visual change
npm run test:tabs:update
git add tests/tabs.spec.js-snapshots/
git commit -m "Update tab test baselines"
```

Tab baselines are stored in `web/tests/tabs.spec.js-snapshots/` and committed to the repo.
CI regenerates them automatically and commits any changes back to the branch.

## Unit tests

```bash
cd web && npm test                                              # Vitest unit tests
dotnet test src/generator/tests/generator.tests.fsproj          # F# generator tests
dotnet test src/SpiroFs/tests/SpiroFs.tests.fsproj              # SpiroFs tests
```

## New worktree

```bash
git worktree add ../dactyl-font-branch <branch>
cd ../dactyl-font-branch
git submodule update --init --recursive
dotnet restore
dotnet tool restore
cd web && npm ci && cd ..
```

## Web directory structure (`web/`)

```
web/
├── src/
│   ├── App.jsx           — Root React component; tab routing, sidebar, worker orchestration
│   ├── SplineEditor.jsx  — Interactive spline editor (Splines tab)
│   ├── SplineGrid.jsx    — Grid view of spline shapes (Spline Grid tab)
│   ├── GrowCanvas.jsx    — WebGL2 field-threshold preview (Generate tab, Bubble mode)
│   ├── growth.js         — Bubble mode engine: distance field + marching-squares contours
│   ├── growthSvg.js      — Bubble mode back end: strokes → field / layered SVG (worker side)
│   ├── glyphSpines.js    — Solves glyph backbones into polylines (Generate tab seed geometry)
│   ├── growthExport.js   — Generate tab PNG/SVG save + clipboard helpers (all three modes)
│   ├── branching.js      — Grow mode engine: space-colonisation branching off the spine
│   ├── branchSvg.js      — Grow mode back end: strokes → branches → SVG (worker side)
│   ├── texture.js        — Texture mode engine: reaction-diffusion / maze / circuit patterns
│   ├── textureSvg.js     — Texture mode back end: strokes → mask → pattern → SVG (worker side)
│   ├── fontExport.js     — OTF font assembly via opentype.js + paper.js boolean union
│   ├── fontExport.test.js — Vitest unit tests for font export
│   ├── worker.js         — Web worker: calls Fable-compiled F# API off the main thread
│   ├── proofs.js         — Proof text data (wrap/strip helpers, book list)
│   ├── proofs/
│   │   ├── lowercase.txt — Lowercase frequency proof text
│   │   ├── uppercase.txt — Uppercase frequency proof text
│   │   └── books.js      — Classic book excerpts for the "Classic" proof mode
│   └── lib/
│       └── fmin/         — Nelder-Mead minimiser (git submodule, used by DactylSpline)
├── tests/
│   ├── tabs.spec.js              — Playwright: screenshot each tab against baselines
│   ├── tweens.spec.js            — Playwright: screenshot each tween axis against baselines
│   ├── font-download.spec.js     — Playwright: OTF download smoke test
│   ├── tabs.spec.js-snapshots/   — Committed baseline PNGs for tab tests
│   ├── tweens.spec.js-snapshots/ — Committed baseline PNGs for tween tests
│   └── font-download.spec.js-snapshots/
├── public/               — Static assets served at root
├── index.html            — Vite entry point
├── vite.config.js        — Vite config (base: '/dactyl-font/', Vitest config)
├── playwright.config.js  — Playwright config (runs against `vite preview`)
└── package.json          — npm scripts and dependencies
```

### Key npm scripts

| Script | What it runs |
|--------|-------------|
| `npm run dev` | (from repo root) Fable watch + Vite dev server |
| `npm run build` | Vite production build to `web/dist/` |
| `npm run preview` | Serve the production build locally |
| `npm test` | Vitest unit tests (`src/**/*.test.js`) |
| `npm run test:tabs` | Playwright tab screenshot tests (needs `npm run build` first) |
| `npm run test:tweens` | Playwright tween screenshot tests |
| `npm run test:font-download` | Playwright OTF download smoke test |
| `npm run test:tabs:update` | Regenerate tab baseline PNGs |

### Worker architecture

`worker.js` runs the Fable-compiled F# on a dedicated Web Worker thread so the main UI thread stays responsive during long solves.  All calls go through a simple promise-based `postMessage` protocol keyed by a sequential `id`.  Progress callbacks post intermediate `{ type: 'progress', value: 0..1 }` messages that App.jsx uses to drive the progress bar.
