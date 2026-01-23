---
description: Run all unit tests for the generator project
---

To run the full test suite:

```bash
dotnet test src/generator/Generator.fsproj
```

To run only the Solver tests (including typical regression tests):

```bash
dotnet test src/generator/Generator.fsproj --filter "FullyQualifiedName~SolverTests"
```

To run Variable Point tests:

```bash
dotnet test src/generator/Generator.fsproj --filter "FullyQualifiedName~VariablePointTests"
```
