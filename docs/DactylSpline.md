# DactylSpline Algorithm Documentation

## Overview
`DactylSpline` is a spline fitting algorithm designed to generate curves that approximate **Euler Spirals** (also known as Cornu spirals or spiros). In an Euler spiral, the curvature ($k$) varies linearly with the arc length ($s$).

This property ($k = ms + c$) creates aesthetically pleasing transitions between curves and straight lines, widely used in font design (e.g., in the work of Raph Levien).

## Core Concepts

### 1. Control Points (`DControlPoint`)
The input to the algorithm is a sequence of control points. Each point can be:
- **Corner**: A sharp corner where continuity is broken.
- **Smooth**: A point where the curve passes through smoothly ($G^2$ continuity).
- **Tangent Constraints**: Points can optionally specify a fixed tangent angle (`th`).
- **Position Constraints**: `x` and `y` are usually fixed, but can be `None` (implied/fitted).

### 2. Spline Segments
The algorithm processes the spline in segments. A "segment" here is defined as a sequence of points between two "Corner" points (or start/end of the spline). The algorithm attempts to fit a **single** continuous curvature profile to this entire sequence.

### 3. Bezier Approximation
The curve is represented internally as a sequence of **Cubic Bézier curves**. Since a single cubic Bézier cannot perfectly represent an Euler spiral, the algorithm optimizes the parameters of these connected Bézier curves to approximate the desired curvature profile.

The parameters for each Bézier point `i` are:
- `x`, `y`: Position (usually fixed).
- `th`: Tangent angle.
- `ld`: Left distance (handle length to previous point).
- `rd`: Right distance (handle length to next point).

### 4. The Optimization Loop (`Solver`)
The `Solver` class performs the fitting.

#### Assessment (Objective Function)
The core logic resides in `computeErr()`.
1.  **Sampling**: Each Bézier segment is sampled at uniform intervals (e.g., 8 steps).
2.  **Curvature Calculation**: At each sample point `j`, the curvature $k_j$ and incremental arc length $\Delta s_j$ are calculated.
3.  **Linear Regression**:
    - The algorithm treats arc length $s$ (cumulative) as the independent variable.
    - Curvature $k$ is the dependent variable.
    - It performs a simple linear regression to fit a line: $k_{approx} = m \cdot s + c$.
4.  **Error Terms**: The total error minimized is a sum of:
    - `residuals`: The sum of squared differences between the actual curvature and the fitted line (measure of "how much like an Euler spiral is this?").
    - `abs m`: The absolute value of the slope. This regularizer encourages **constant curvature** (circles) over spirals.
    - `max_dist`: (Debug/Optional) Sometimes minimizes total length.
    - `continuity`: If closed, error includes mismatch between start/end curvature.

#### Minimization
The algorithm uses the **Nelder-Mead** (simplex) method to iteratively adjust the free parameters (tangents and handle lengths) of the Bézier points to minimize the error function.

## Execution Flow
1.  **Split**: The input sequence of points is split into chunks bounded by `Corner` points.
2.  **Solve**: For each chunk:
    - Initialize Bézier points.
    - Run Nelder-Mead optimization to find best `th`, `ld`, `rd`.
3.  **Render**: Convert the optimized Bézier points into SVG `path` commands (`C` curve-to).

## Current Behavior & Limitations
- The algorithm enforces a single linear curvature trend ($k = ms+c$) across *all* smooth points between corners. This means if you have multiple points forming an "S" shape (crossing zero curvature), it fits one line. If you have "straight-curve-straight", it might struggle if the transition implies a complex piecewise curvature profile not matching a single line.
- The term `abs m` might aggressively flatten spirals into circular arcs.
