
**TODOs**
- fix P/R '.' joins
- add proper interactive spline editor (tab)
- fix italics e.g. a
- add 'filled' checkbox to splines tab, default off
- tidy up UI
- add documentation tooltips, links
- flare with tangents wrong way round
- move outline point inward only
- improve serifs
- join lines properly
- correct tight bend in '5'
- render animation
- try merging with https://magenta.tensorflow.org/svg-vae
- add punctuation chars
- 'bowtie' where lines all cross
- mark joins to remove serifs
- generate proofs, ideally using @font-face
- calculate kerning
- from https://www.typography.com/blog/typographic-illusions:
-  overshoot
-  balance (mid height > 1/2)
- Add help tooltips on explorer sliders
- fix fontforge errors: direction, non-integral coords
- fix serifs: curve joints, check Y{}, spacing
- Optional debug mode to show coordinates/curves
- debug 'sharp bend' duplicate points in 'e'
- mobius strip font
-
**DONE: Implemented Features**
- Backscratch font (made of 4 parallel lines)
- Generated FontForge fonts
- Variable font explorer: https://terryspitz.github.io/dactyl-font/
- Mono (fixed-width) font
- Horiz/vertical endcaps using axis_align_caps
- Randomise in explorer
- Contrast (horiz vs vert stroke width ratio)
- Flared endcaps
- Constrain tangents to horiz/vertical
- Italics subdivide splines to ensure better fit
- Dactyl-smooth which has no corners

**Document if interesting the following categories**
- Straights: AEFHIKLMNTVWXYZklvwxyz147/=[]\`|*"'
- Dots: ij:;!?
- Curves: COScos36890()~
- LeftUpright: BDPRb mnpr
- RightUpright: GJadgq
- Other: QUefhtu25@#$€£_&-+{}%