# Claude Code guidance for dactyl-font

## Repository

GitHub repo: `terryspitz/dactyl-font`

## GitHub interaction

**No `gh` CLI available.** Use the `mcp__github__*` tools for all GitHub
operations (viewing PRs, posting comments, checking CI, etc.).  Use
`ToolSearch` to load the schema for any `mcp__github__` tool before calling it.

## Branch conventions

Development branches: `claude/**` (e.g. `claude/fix-space-width-font-xQEFZ`).
Push to the designated feature branch; do not push directly to `main`.

## Build pipeline

The web app depends on F# compiled to JavaScript via [Fable](https://fable.io/).
`dotnet` is **not available** in this environment, so the F# → JS compilation
step must run in CI.

Full build sequence (CI only):
```
dotnet restore && dotnet tool restore
dotnet fable src/explorer --outDir web/src/lib/fable   # F# → JS
cd web && npm ci && npm run build                       # Vite production build
```

## Tests: local vs CI

### Runnable locally (no dotnet needed)
These work against the compiled JS in `web/src/lib/fable/` — but only if that
directory has been populated by a prior Fable build or by copying from CI.

| Command | What it does |
|---------|-------------|
| `cd web && npm test` | Vitest unit tests (`src/**/*.test.js`) |

### Require CI (need Fable-compiled output + production build)

| CI workflow | Trigger | What it does |
|-------------|---------|-------------|
| `.NET Core` (`dotnet-core.yml`) | push to `main` or `claude/**`; PRs to `main` | `dotnet build` + `dotnet test` (F# unit tests) |
| `Visual Tests` (`visual-tests.yml`) | PRs to `main`; `workflow_dispatch` | Playwright screenshot tests against production build; auto-commits new baselines |
| `Deploy Pages` (`deploy-pages.yml`) | push to `main` only | Builds and deploys to GitHub Pages |

**Visual test snapshots** live in `web/tests/tabs.spec.js-snapshots/` and
`web/tests/tweens.spec.js-snapshots/` (committed to the repo).  After
intentional visual changes, trigger the `Visual Tests` workflow manually via
`workflow_dispatch` with `update_snapshots: true` to regenerate and
auto-commit them.

To run visual tests manually (requires dotnet + Node + Playwright):
```
dotnet fable src/explorer --outDir web/src/lib/fable
cd web && npm ci && npx playwright install chromium && npm run build
npm run test:tabs            # compare snapshots
npm run test:tabs:update     # regenerate snapshots
```

## Key source files

| File | Purpose |
|------|---------|
| `src/generator/Font.fs` | Core font geometry — glyph shapes, advance widths, space width |
| `src/generator/Axes.fs` | Font axis definitions and defaults |
| `src/generator/GeneratorTypes.fs` | Element type definitions (`Element`, `movePoints`, etc.) |
| `src/explorer/Api.fs` | Fable-compiled API surface exposed to JS (`generateFontGlyphData`, etc.) |
| `web/src/App.jsx` | Main React UI — proof rendering, font size, tab layout |
| `web/src/worker.js` | Web worker that calls the compiled Fable API |
| `web/src/proofs.js` | Proof text helpers and wrap function |
| `web/src/fontExport.js` | OTF font assembly from glyph data |
