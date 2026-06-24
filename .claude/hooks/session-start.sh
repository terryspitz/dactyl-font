#!/bin/bash
set -euo pipefail

# Only run in Claude Code on the web
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Install .NET 8 SDK if not present
if ! command -v dotnet &>/dev/null; then
  echo "Installing .NET 8 SDK..."
  apt-get update -q || true
  apt-get install -y dotnet-sdk-8.0
fi

# Initialize git submodules (fmin is a submodule under web/src/lib/fmin)
git submodule update --init --recursive

# Restore .NET packages and tools
dotnet restore
dotnet tool restore

# Install root npm dependencies (concurrently)
# Use `npm ci` so the lockfiles are installed verbatim and never rewritten
# (`npm install` can mutate package-lock.json, e.g. dropping libc metadata,
# leaving spurious uncommitted changes every session).
npm ci

# Install web npm dependencies
npm ci --prefix web
