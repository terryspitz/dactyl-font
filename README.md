NOTE: This README is best viewed at https://terryspitz.github.io/dactyl-font/README.html

# Dactyl Font Generator

## tl;dr

Play with the [Dactyl Live explorer](https://terryspitz.github.io/dactyl-font) — explore fonts interactively, or switch to the **Glyphs** tab to see how Dactyl's glyph string definitions convert to different letters.

See some of the chequered development history at [Dactyl Font Gallery](gallery.html), and some sample fonts at [Sample fonts](allGlyphs.html)

## What & Why

Dactyl is a functional font generator written in F#, in service of ticking 'create a font' off my bucket list!

Dactyl is parameterised with a large (and growing) number of inputs including 'axes' (parameters) common in variable fonts such as stroke thickness and x-height, and more unusual ones like roundness and end-bulbs.

It's an exercise in understanding how fonts can be built in code using both ['words and rules'](https://en.wikipedia.org/wiki/Words_and_Rules) — where the 'rules' are a minimal set of general shape logic in code along with the 'words': a minimal set of hardcoded shape data per glyph (letter).

As well as generating pretty and super-controllable fonts, this work will hopefully also help me improve my AI font generator [DeeperFont](https://github.com/terryspitz/ipython_notebooks/tree/master/deeper), though at the moment the technologies are quite different and combining them will be an interesting future challenge.

You can explore Dactyl live in your browser using the [Dactyl Live explorer](https://terryspitz.github.io/dactyl-font).  Note the Reset and Random icons at the top. The settings deliberately extend beyond 'normal' values to show how the fonts behave (and often misbehave) under extremes.

A few predefined Dactyl fonts are available to download in the [ttf](https://github.com/terryspitz/dactyl-font/tree/SpiroFs/ttf){:target="_blank" rel="noopener"} subdirectory. Note: these fonts are still a work in progress and do not have good spacing or kerning, or even good shapes under some settings. In future I plan to make fonts downloadable after customising in the live browser.

## Documentation

- [DactylGlyph Documentation](docs/DactylGlyphs.md) — How glyph string definitions work
- [DactylSpline Documentation](docs/DactylSpline.md) — The DactylSpline curve implementation

## Spiro Curves

Dactyl fonts initially used and were inspired by Ralph Levien's [Spiro curves](https://www.levien.com/spiro/). I used Wiesław Šoltés's [C# port](https://github.com/wieslawsoltes/SpiroNet), which I've ported to F# to run under the fantastic [Fable](https://fable.io/) to transpile to javascript, meaning I can write the whole thing in beautiful F#.

Raph superseded his v1 Spiro curves in 2018 with [spline-research](https://github.com/raphlinus/spline-research), described in [this blog post](https://raphlinus.github.io/curves/2018/12/21/new-spline.html), which are available with the 'spline2' checkbox. These offer direct control of tangents specifically to help font design (this feature was also later added to Spiro curves as Anchor/Handles). See [Raph's talk](https://www.youtube.com/watch?v=eqNngVkMBzE).

Later splines which I'd like to investigate:

- [κ-Curves: Interpolation at Local Maximum Curvature](https://people.engr.tamu.edu/schaefer/research/kcurves.pdf)
- [Enhancing flexibility and control in κ-curve using fractional Bézier curves](https://www.sciencedirect.com/science/article/pii/S1110016824000589)

## Variable Fonts

Dactyl is also inspired by Variable Fonts technology. Unfortunately it isn't easy to output a Dactyl font as a variable font since they can only interpolate between two glyphs with the exact same knot points, which is hard to achieve with Dactyl's spiro bezier curves.

- [v-fonts.com](https://v-fonts.com/) — Interactive variable font playground
- [MDN Variable Fonts Guide](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Fonts/Variable_Fonts_Guide) — "The five registered axes are weight, width, slant, italic, and optical size."
- [Google: Variable Fonts](https://developers.google.com/web/fundamentals/design-and-ux/typography/variable-fonts)
- [Play Type Detail](https://play.typedetail.com)
- [Axis-Praxis](https://www.axis-praxis.org) — Variable font sandbox
- [A List Apart: UI for Variable Fonts](https://alistapart.com/article/user-interfaces-for-variable-fonts/)

## Font Creation / Import Formats

[FontForge](https://fontforge.org/en-US) is an open-source font editor which supports Spiro.

Since none of the font interop formats I found support spiros directly, the Dactyl generator writes files in FontForge native format (a .fsdir directory of .glyph files), which can be loaded into FontForge and used to generate TTF fonts. FontForge is also useful to view and interact with the letter glyphs in detail: it has great validation, viewing and editing features.

- [Design With FontForge: Importing Glyphs](http://designwithfontforge.com/en-US/Importing_Glyphs_from_Other_Programs.html)
- [Unified Font Object (UFO3) Spec](http://unifiedfontobject.org/versions/ufo3/glyphs/glif)
- [OpenType.js Glyph Inspector](https://opentype.js.org/glyph-inspector.html)

## Font Design References

This project is a great opportunity to dive into the vast world of typographic design. A few sites and articles that have caught my eye:

- [Design With FontForge: Creating o and n](http://designwithfontforge.com/en-US/Creating_o_and_n.html)
- [Design With FontForge: Trusting Your Eyes](http://designwithfontforge.com/en-US/Trusting_Your_Eyes.html)
- [Creating Fonts with Inkscape and FontForge](https://www.reddit.com/r/neography/comments/8186cc/creating_fonts_with_inkscape_and_fontforge_part1) (Reddit)
- [Typography.com: Text for Proofing Fonts](https://www.typography.com/blog/text-for-proofing-fonts)

On letter spacing, tracking and kerning:

- [Fernando Mello: Spacing Essay (PDF)](http://www.fermello.org/FernandoMello_essay.pdf)
- [FontShop: Adventures in Space](https://www.fontshop.com/content/adventures-in-space_spacing)
- [Kerning Test](https://typefacts.com/artikel/kerningtest)
- [Typography.com: Typographic Illusions](https://www.typography.com/blog/typographic-illusions)
- [Typography.com: Turning Type Sideways](https://www.typography.com/blog/turning-type-sideways)
- [Google Fonts](https://design.google/library/google-fonts)

## Where It All Started

[Donald Knuth's MetaFont paper](http://www.math.lsa.umich.edu/~millerpd/docs/501_Winter13/Knuth79.pdf)
