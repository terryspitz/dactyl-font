---
name: windows-shell-reliability
description: "Reliable command execution on Windows: paths, encoding, and common binary pitfalls."
risk: low
source: community
date_added: "2026-03-19"
---

# Windows Shell Reliability Patterns

> Best practices for running commands on Windows via PowerShell and CMD.

---

## 1. Encoding & Redirection

### CRITICAL: UTF-16LE Output
Many Windows tools (like `dotnet build` or `npm`) output in UTF-16LE or other encodings that can break subsequent processing.

| Problem | Symptom | Solution |
|---------|---------|----------|
| `dotnet > log.txt` | `view_file` fails | `Get-Content log.txt | Set-Content -Encoding utf8 log_utf8.txt` |
| `npm run > log.txt` | Encoding error | Use PowerShell piping: `npm run ... | Out-File -Encoding UTF8 log.txt` |

**Rule:** Always convert redirected output to UTF-8 if it needs to be read back or processed by other tools.

---

## 2. Handling Paths & Spaces

### CRITICAL: Quoting
Windows paths often contain spaces.

| ❌ Wrong | ✅ Correct |
|----------|-----------|
| `dotnet build src/my project/file.fsproj` | `dotnet build "src/my project/file.fsproj"` |
| `& C:\Path With Spaces\bin.exe` | `& "C:\Path With Spaces\bin.exe"` |

**Rule:** Always quote absolute and relative paths that may contain spaces.

### The Call Operator (&)
In PowerShell, if an executable path starts with a quote, you MUST use the `&` operator.

**Pattern:**
```powershell
& "C:\Program Files\dotnet\dotnet.exe" build ...
```

---

## 3. Common Binary & Cmdlet Pitfalls

| Action | ❌ CMD Style | ✅ PowerShell Choice |
|--------|-------------|---------------------|
| Delete | `del /f /q file` | `Remove-Item -Force file` |
| Copy | `copy a b` | `Copy-Item a b` |
| Move | `move a b` | `Move-Item a b` |
| Make Dir | `mkdir folder` | `New-Item -ItemType Directory -Path folder` |

**Tip:** Using CLI aliases like `ls`, `cat`, and `cp` in PowerShell is usually fine, but using full cmdlets in scripts is more robust.

---

## 4. Dotnet CLI Reliability

### Build Speed & Consistency
| Context | Command | Why |
|---------|---------|-----|
| Fast Iteration | `dotnet build --no-restore` | Skips redundant nuget restore. |
| Clean Build | `dotnet build --no-incremental` | Ensures no stale artifacts. |
| Background | `dotnet run > output.txt 2>&1` | Captures both stdout and stderr. |

---

## 5. Environment Variables

| Shell | Syntax |
|-------|--------|
| PowerShell | `$env:VARIABLE_NAME` |
| CMD | `%VARIABLE_NAME%` |

---

## 6. Long Paths
Windows has a 260-character path limit by default.

**Fix:** If you hit long path errors, use the extended path prefix:
`\\?\C:\Very\Long\Path\...`

---

## 7. Troubleshooting Shell Errors

| Error | Likely Cause | Fix |
|-------|-------------|-----|
| `The term 'xxx' is not recognized` | Path not in $env:PATH | Use absolute path or fix PATH. |
| `Access to the path is denied` | File in use or permissions | Stop process or run as Admin. |
| `Encoding mismatch` | UTF-16 output | Use `Out-File -Encoding UTF8`. |

---
