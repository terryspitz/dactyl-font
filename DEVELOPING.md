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

# Run tween screenshot tests (generates per-axis screenshots)
npm run test:tweens

# Regenerate tab baselines after an intentional visual change
npm run test:tabs:update
git add tests/tabs.spec.js-snapshots/
git commit -m "Update tab test baselines"
```

Tab baselines are stored in `web/tests/tabs.spec.js-snapshots/` and committed to the repo.
CI regenerates them automatically and commits any changes back to the branch.

## New worktree

```bash
git worktree add ../dactyl-font-branch <branch>
cd ../dactyl-font-branch
git submodule update --init --recursive
dotnet restore
dotnet tool restore
cd web && npm ci && cd ..
```
