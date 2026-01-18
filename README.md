NOTE: This README is best viewed at https://terryspitz.github.io/dactyl-font/README.html

# Dactyl Live

Play with the [Dactyl Live explorer](https://terryspitz.github.io/dactyl-font)

or explore individual glyph string definitions and how they convert to the different spline choices by switching to the **Splines** tab in the [Dactyl Live explorer](https://terryspitz.github.io/dactyl-font).

View some sample Dactyl fonts [here](allGlyphs.html)

# Dactyl: What & Why

Dactyl is a functional font generator written in F#, in service of ticking 'create a font' off my bucket list!  Dactyl is parameterised with a large (and growing) number of inputs including 'axes' (parameters) common in variable fonts such as stroke thickness and x-height and more unusual ones like roundness and end-bulbs.  Dactyl is an exercise in understanding how fonts can be built in code using both ['words and rules'](https://en.wikipedia.org/wiki/Words_and_Rules) - where the 'rules' are a minimal set of general shape logic in code along with the 'words': a minimal set of hardcoded shape data per glyph (letter).  As well as generating pretty and super-controllable fonts this work will hopefully also help me improve my AI font generator [DeeperFont](https://github.com/terryspitz/ipython_notebooks/tree/master/deeper) though at the moment the technologies are quite different and combining them will be an interesting future challenge.

You can explore Dactyl live in your browser using the [Dactyl Live explorer](https://terryspitz.github.io/dactyl-font).  Note the Reset and Random icons at the top.  The settings deliberately extend beyond 'normal' to show how the fonts behave (and often misbehave) under extremes. There is also a **Splines** tab for exploring glyph string definitions.

Here is some of Dactyl's checkered development history:

[View Dactyl Font Gallery](gallery.html)


A few predefined Dactyl fonts are available to download in the [ttf](https://github.com/terryspitz/dactyl-font/tree/SpiroFs/ttf){:target="_blank" rel="noopener"} subdirectory.  Note: these fonts are still a work in progress and do not have good spacing or kerning, or even good shapes under some settings.  In future I plan to make fonts downloadable afte customising in the live browser.  Some sample fonts are [here](allGlyphs.html).

## Spiro curves

Dactyl fonts use and are inspired by Ralph Levien's [Spiro curves](https://www.levien.com/spiro/).  I used Wiesław Šoltés's [C# port](https://github.com/wieslawsoltes/SpiroNet)
which I've ported to F# to run under the fantastic [Fable](https://fable.io/) to transpile to javascript, meaning I can write the whole thing in beautiful F#.

Raph superceded his spiro curves in 2018 with https://github.com/raphlinus/spline-research, described in https://raphlinus.github.io/curves/2018/12/21/new-spline.html, which are available with the 'spline2' checkbox.  These offer direct control of tangents specifically to help font design (this feature was also later added to Spiro curves as Anchor/Handles). See https://www.youtube.com/watch?v=eqNngVkMBzE

Later splines which I'd like to investigate are:

- [κ-Curves: Interpolation at Local Maximum Curvature](https://people.engr.tamu.edu/schaefer/research/kcurves.pdf)
- [Enhancing flexibility and control in κ-curve using fractional Bézier curves](https://www.sciencedirect.com/science/article/pii/S1110016824000589)

## Variable fonts

Dactyl is also inspired by Variable Fonts technology.  Unfortunately though it isn't easy to output a Dactyl font as a variable font since they can only interpolate between two glyphs with the exact same knot points, which is hard to achieve with Dactyl's spiro bezier curves.

<https://v-fonts.com/>

<https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Fonts/Variable_Fonts_Guide>

"The five registered axes are weight, width, slant, italic, and optical size."

<https://developers.google.com/web/fundamentals/design-and-ux/typography/variable-fonts>

<https://play.typedetail.com>

<https://www.axis-praxis.org>

<https://alistapart.com/article/user-interfaces-for-variable-fonts/>


## Font creation/import formats

[FontForge](https://fontforge.org/en-US) is an open-source font editor which supports Spiro.  

Since none of the font interop formats I found support spiros directly the Dactyl generator writes files in FontForge native format (a .fsdir directory of .glyph files), which can be loaded into FontForge and used generate TTF fonts.  
FontForge is also useful to view and interact with the letter glyphs in detail: it has great validation, viewing and editing features.

<http://designwithfontforge.com/en-US/Importing_Glyphs_from_Other_Programs.html>

<http://unifiedfontobject.org/versions/ufo3/glyphs/glif>

<https://opentype.js.org/glyph-inspector.html>

## Font design

This project is a great opportunity to dive into the vast world of typographic design.  A few of the sites and articles that have caught my eye so far are below.  

<http://designwithfontforge.com/en-US/Creating_o_and_n.html>

<http://designwithfontforge.com/en-US/Trusting_Your_Eyes.html>

<https://www.reddit.com/r/neography/comments/8186cc/creating_fonts_with_inkscape_and_fontforge_part1>

<https://www.typography.com/blog/text-for-proofing-fonts>

On letter spacing, tracking and kerning: 

<http://www.fermello.org/FernandoMello_essay.pdf>

<https://www.fontshop.com/content/adventures-in-space_spacing>

<https://typefacts.com/artikel/kerningtest>

<https://www.typography.com/blog/typographic-illusions>

<https://www.typography.com/blog/turning-type-sideways>

<https://design.google/library/google-fonts>



## Where it all started

[Donald Knuth's MetaFont paper](http://www.math.lsa.umich.edu/~millerpd/docs/501_Winter13/Knuth79.pdf)

