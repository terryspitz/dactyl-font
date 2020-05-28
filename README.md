A functional font generator written in F#, in service of ticking 'create a font' off my bucket list.

Dactyl fonts are available in the [ttf](https://github.com/terryspitz/dactyl-font/tree/SpiroFs/ttf){:target="_blank" rel="noopener"} subdirectory Note: these fonts are still a work in progress and do not have good spacing or kerning, or even good shapes under some settings.

You can explore Dactyl live in your browser using the [Dactyl Live explorer](https://terryspitz.github.io/dactyl-font/explorer/public/index.html){:target="_blank" rel="noopener"}

See some of Dactyl's checkered past in this animation:

![font development](png/font.gif)

And current samples:

![font samples](allGlyphs.svg)

## Spiro spiral curves

Dactyl fonts use and are inspired by Ralph Levien's [Spiro curves](https://www.levien.com/spiro/)
using Wiesław Šoltés's [C# port](https://github.com/wieslawsoltes/SpiroNet)
which I've ported to F# to run under the fantastic [Fable](https://fable.io/) to transpile to javascript.

## Variable fonts

Dactyl is also inspired by Variable Fonts technology although it unfortunately isn't easy to output as a variable font
(since they can only interpolate between two glyphs with the exact same knot points, which is hard to achieve with
Spiros-created bezier curves.)

<https://v-fonts.com/>

<https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Fonts/Variable_Fonts_Guide>

"The five registered axes are weight, width, slant, italic, and optical size."

<https://developers.google.com/web/fundamentals/design-and-ux/typography/variable-fonts>

<https://play.typedetail.com>

<https://www.axis-praxis.org>


## Font creation/import formats

[FontForge](https://fontforge.org/en-US) is an open-source font editor which supports Spiro.  

Since none of the font interop formats I found support spiros the Dactyl generator writes files in FontForge native format (a .fsdir directory of .glyph files), which I can load into FontForge and generate TTF fonts.  
FontForge is also useful to view and interact with the letter glyphs in detail: it has great validation, viewing and editing features.

<http://designwithfontforge.com/en-US/Importing_Glyphs_from_Other_Programs.html>

<http://unifiedfontobject.org/versions/ufo3/glyphs/glif>


## Font design

This project is a great opportunity to dive into the vast world of typographic design.  A few of the sites and articles that
have caught my eye so far:

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

