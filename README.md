# Dactyl: What & Why

Dactyl is a functional font generator written in F#, in service of ticking 'create a font' off my bucket list, in style!  Fonts are parameterised with a large (and growing) number of inputs including axes common in variable fonts such as stroke thickness and x-height and more unusual ones like roundness and end-bulbs.  Dactyl is an exercise in understanding how fonts can be built in code using both ['words and rules'](https://en.wikipedia.org/wiki/Words_and_Rules) - a minimal set of general shape logic along with a minimal set of shape data per glyph.  This work will hopefully help me improve my AI font generator [DeeperFont](https://github.com/terryspitz/ipython_notebooks/tree/master/deeper) though at the moment the technologies are quite different and combining them will be an interesting future challenge.

You can explore Dactyl live in your browser using the [Dactyl Live explorer](https://terryspitz.github.io/dactyl-font/explorer/public/index.html){:target="_blank" rel="noopener"}.  Note the Reset and Random icons next to the title.  The settings deliberately extend beyond 'normal' to show how the fonts behave (and often misbehave) under extremes.

See some of Dactyl's checkered past in this animation:

![font development](png/font.gif)

A few predefined Dactyl fonts are available to download in the [ttf](https://github.com/terryspitz/dactyl-font/tree/SpiroFs/ttf){:target="_blank" rel="noopener"} subdirectory.  Note: these fonts are still a work in progress and do not have good spacing or kerning, or even good shapes under some settings.  In future I plan to make fonts downloadable afte customising in the live browser.  Here are some samples:

![font samples](allGlyphs.svg)

## Spiro curves

Dactyl fonts use and are inspired by Ralph Levien's [Spiro curves](https://www.levien.com/spiro/).  I used Wiesław Šoltés's [C# port](https://github.com/wieslawsoltes/SpiroNet)
which I've ported to F# to run under the fantastic [Fable](https://fable.io/) to transpile to javascript.

Raph superceded his spiro curves in 2018 with https://github.com/raphlinus/spline-research.  This might be a future direction for Dactyl as it offers direct control of tangents specifically to help font design.

## Variable fonts

Dactyl is also inspired by Variable Fonts technology.  Unfortunately though it isn't easy to output a Dactyl font as a variable font since they can only interpolate between two glyphs with the exact same knot points, which is hard to achieve with Dactyl's spiro bezier curves.

<https://v-fonts.com/>

<https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Fonts/Variable_Fonts_Guide>

"The five registered axes are weight, width, slant, italic, and optical size."

<https://developers.google.com/web/fundamentals/design-and-ux/typography/variable-fonts>

<https://play.typedetail.com>

<https://www.axis-praxis.org>


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

## Where it all started

[Donald Knuth's MetaFont paper](http://www.math.lsa.umich.edu/~millerpd/docs/501_Winter13/Knuth79.pdf)

