A functional font generator written in F#, in service of ticking 'create a font' off my bucket list.

Dactyl fonts are available in the [ttf/](https://github.com/terryspitz/dactyl-font/tree/SpiroFs/ttf){:target="_blank" rel="noopener"} subdirectory (note: these fonts are still a work in progress and do not have good spacing or kerning.)

You can explore Dactyl live in your browser using the [Live Explorer](https://terryspitz.github.io/dactyl-font/explorer/public/index.html){:target="_blank" rel="noopener"}

See some of Dactyl's checkered past in this animation:

![font development](png/font.gif)

And current samples:

![font samples](allGlyphs.svg)

## Spiro spiral curves

Dactyl fonts use and are inspired by Ralph Levien's [Spiro curves](https://www.levien.com/spiro/)

using Wiesław Šoltés's [C# port](https://github.com/wieslawsoltes/SpiroNet)

which I've ported to F# to run under the fantastic [Fable](https://fable.io/) to transpile to javascript.

## Variable fonts

Inspired by Variable Fonts technology

https://v-fonts.com/

https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Fonts/Variable_Fonts_Guide

"The five registered axes are weight, width, slant, italic, and optical size."

https://developers.google.com/web/fundamentals/design-and-ux/typography/variable-fonts

https://play.typedetail.com

https://www.axis-praxis.org


## Font import formats

[FontForge](https://fontforge.org/en-US) is an open-source font editor which supports Spiro.  

Since font interop formats don't seem to support spiros I write files in FontForge native format,
then import and generate TTF fonts.  
FontForge is useful to interact with the letter glyphs: it has great validation, viewing and editing features.

http://designwithfontforge.com/en-US/Importing_Glyphs_from_Other_Programs.html

http://unifiedfontobject.org/versions/ufo3/glyphs/glif


## Font design

http://designwithfontforge.com/en-US/Creating_o_and_n.html

http://designwithfontforge.com/en-US/Trusting_Your_Eyes.html

https://www.reddit.com/r/neography/comments/8186cc/creating_fonts_with_inkscape_and_fontforge_part1/