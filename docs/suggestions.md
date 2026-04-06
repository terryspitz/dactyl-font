# DactylSpline Optimization Suggestions

## Analysis of Current Algorithm
The current algorithm attempts to fit a **single Euler spiral** (where curvature $k$ is linear with arc length $s$, i.e., $k = ms + c$) to an entire chain of "Smooth" points bounded by "Corners".

### Issues
1.  **Single Spiral Limitation**: Complex curves (e.g., "straight-curve-straight" or composite curves) may not fit well to a single linear curvature function. This forces the solver to compromise, potentially resulting in uneven curvature or poor fitting.
2.  **Global Optimization**: By optimizing the entire chain at once against a single line, local variations might be smoothed out aggressively.

## Suggestions

### 1. Piecewise Euler Spiral Fitting
Instead of fitting one spiral to the whole chain, fit **piecewise Euler spirals**.
- Treat each segment between two control points (knots) as a distinct Euler spiral segment.
- **Constraints**: Enforce $G^2$ continuity (position, tangent, and curvature continuity) at the knots.
- **Optimization**: This is effectively solving for the "correct" tangents and curvatures at the knots such that the segments between them are Euler spirals. This aligns with Raph Levien's "Spline" logic.

### 2. Curvature Extrema Alignment
Standard font design practice usually places control points at the **extrema of curvature** (or at x/y extrema).
- **Suggestion**: If we assume users are placing points at curvature extrema, we can enforce constraints that $dk/ds = 0$ at specific points, or optimize knots such that extrema align with them.
- However, for an Euler spiral segment, curvature is monotonic. Max curvature *must* be at one of the ends.
- **Implication**: Any "hump" in curvature (peak in the middle) requires *at least two* Euler spiral segments joined at the peak.
- **Action**: Encourage or enforce that curve segments between knots are monotonic in curvature. If a user defines a segment that *needs* a peak in the middle, it should ideally be split into two segments.

### 3. Objective Function Tuning
- **`abs m` (Flatness)**: Currently minimizes the slope of the curvature graph. This pushes curves towards circles ($m=0$). Tuning this weight allows balancing "spirality" vs "circularity".
- **Residuals**: Ensure we weigh position error sufficiently high so the curve passes *near* the control points, even if curvature is optimized.

### 4. Interactive Feedback
- Use the new **Curvature Comb** to visually inspect where the algorithm fails to produce smooth transitions. Look for "jumps" in the comb height at knots (discontinuous curvature) or wobbles (bad fit).

---

## Implemented Improvements (from Bespoke Splines paper analysis)

Based on analysis of Raph Levien's ["Bespoke Splines" paper](https://spline.technology/paper1.pdf), the following improvements have been implemented:

### 1. Analytical Arm Lengths (`bespoke_arms` setting)
The key insight from the paper: given tangent angles θ₀, θ₁ relative to the chord between two control points, the arm lengths of the cubic Bézier are **determined analytically** — they don't need to be free optimization variables. This is implemented via `bespokeArmLength`/`bespokeArmLengths` functions using the same formula as `myCubic` in curves.fs.

**Benefits:**
- Reduces the parameter space by nearly half (eliminating `ld`/`rd` from optimization)
- Each Nelder-Mead evaluation is faster and converges more reliably
- The resulting curves more closely approximate Euler spirals

### 2. Fast Iterative Solver (`iterative_solver` setting)
Replaces Nelder-Mead with an iterative Newton-step solver that adjusts tangent angles to achieve curvature continuity at joins. Similar to `TwoParamSpline.iterDumb` in curves.fs.

**Benefits:**
- O(n) per iteration vs O(n²) for Nelder-Mead
- Typically converges in 10-20 iterations vs 500+ for Nelder-Mead
- Requires `bespoke_arms=true` to use

### 3. Curvature Blending at Fixed-Tangent Joins
At smooth points with fixed tangents, endpoint curvatures from adjacent segments may not match. Curvature blending adjusts arm lengths using the harmonic mean of the two endpoint curvatures (from Spline2.computeCurvatureBlending).

**Benefits:**
- Smoother transitions at points where the user has specified explicit tangent angles
- Automatically activated when `bespoke_arms=true`
