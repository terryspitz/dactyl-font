NOTE: This README is best viewed at https://terryspitz.github.io/dactyl-font/README.html

# Dactyl Live

Play with the [Dactyl Live explorer](https://terryspitz.github.io/dactyl-font)

or explore individual glyph string definitions and how they convert to the different spline choices by switching to the **Splines** tab in the [Dactyl Live explorer](https://terryspitz.github.io/dactyl-font).

View some sample Dactyl fonts [here](allGlyphs.html)

# Dactyl: What & Why

Dactyl is a functional font generator written in F#, in service of ticking 'create a font' off my bucket list!  Dactyl is parameterised with a large (and growing) number of inputs including 'axes' (parameters) common in variable fonts such as stroke thickness and x-height and more unusual ones like roundness and end-bulbs.  Dactyl is an exercise in understanding how fonts can be built in code using both ['words and rules'](https://en.wikipedia.org/wiki/Words_and_Rules) - where the 'rules' are a minimal set of general shape logic in code along with the 'words': a minimal set of hardcoded shape data per glyph (letter).  As well as generating pretty and super-controllable fonts this work will hopefully also help me improve my AI font generator [DeeperFont](https://github.com/terryspitz/ipython_notebooks/tree/master/deeper) though at the moment the technologies are quite different and combining them will be an interesting future challenge.

You can explore Dactyl live in your browser using the [Dactyl Live explorer](https://terryspitz.github.io/dactyl-font).  Note the Reset and Random icons at the top.  The settings deliberately extend beyond 'normal' to show how the fonts behave (and often misbehave) under extremes. There is also a **Splines** tab for exploring glyph string definitions.

Here is some of Dactyl's checkered development history:

<!-- Fotorama from CDNJS, 19 KB -->
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
<link  href="https://cdnjs.cloudflare.com/ajax/libs/fotorama/4.6.4/fotorama.css" rel="stylesheet">
<script src="https://cdnjs.cloudflare.com/ajax/libs/fotorama/4.6.4/fotorama.js"></script>
<div class="fotorama" data-autoplay="300" data-transition="crossfade">
  <img src="png/font (1).png">
  <img src="png/font (2).png">
  <img src="png/font (3).png">
  <img src="png/font (4).png">
  <img src="png/font (5).png">
  <img src="png/font (6).png">
  <img src="png/font (7).png">
  <img src="png/font (8).png">
  <img src="png/font (9).png">
  <img src="png/font (10).png">
  <img src="png/font (11).png">
  <img src="png/font (12).png">
  <img src="png/font (13).png">
  <img src="png/font (14).png">
  <img src="png/font (15).png">
  <img src="png/font (16).png">
  <img src="png/font (17).png">
  <img src="png/font (18).png">
  <img src="png/font (19).png">
  <img src="png/font (20).png">
  <img src="png/font (21).png">
  <img src="png/font (22).png">
  <img src="png/font (23).png">
  <img src="png/font (24).png">
  <img src="png/font (25).png">
  <img src="png/font (26).png">
  <img src="png/font (27).png">
  <img src="png/font (28).png">
  <img src="png/font (29).png">
  <img src="png/font (30).png">
  <img src="png/font (31).png">
  <img src="png/font (32).png">
  <img src="png/font (33).png">
  <img src="png/font (34).png">
  <img src="png/font (35).png">
  <img src="png/font (36).png">
  <img src="png/font (37).png">
  <img src="png/font (38).png">
  <img src="png/font (39).png">
  <img src="png/font (40).png">
  <img src="png/font (41).png">
  <img src="png/font (42).png">
  <img src="png/font (43).png">
  <img src="png/font (44).png">
  <img src="png/font (45).png">
  <img src="png/font (46).png">
  <img src="png/font (47).png">
  <img src="png/font (48).png">
  <img src="png/font (49).png">
  <img src="png/font (50).png">
  <img src="png/font (51).png">
  <img src="png/font (52).png">
  <img src="png/font (53).png">
  <img src="png/font (54).png">
  <img src="png/font (55).png">
  <img src="png/font (56).png">
  <img src="png/font (57).png">
  <img src="png/font (58).png">
  <img src="png/font (59).png">
  <img src="png/font (60).png">
  <img src="png/font (61).png">
  <img src="png/font (62).png">
  <img src="png/font (63).png">
  <img src="png/font (64).png">
  <img src="png/font (65).png">
  <img src="png/font (66).png">
  <img src="png/font (67).png">
  <img src="png/font (68).png">
  <img src="png/font (69).png">
  <img src="png/font (70).png">
  <img src="png/font (71).png">
  <img src="png/font (72).png">
  <img src="png/font (73).png">
  <img src="png/font (74).png">
  <img src="png/font (75).png">
  <img src="png/font (76).png">
  <img src="png/font (77).png">
  <img src="png/font (78).png">
  <img src="png/font (79).png">
  <img src="png/font (80).png">
  <img src="png/font (81).png">
  <img src="png/font (82).png">
  <img src="png/font (83).png">
  <img src="png/font (84).png">
  <img src="png/font (85).png">
  <img src="png/font (86).png">
  <img src="png/font (87).png">
  <img src="png/font (88).png">
  <img src="png/font (89).png">
  <img src="png/font (90).png">
  <img src="png/font (91).png">
  <img src="png/font (92).png">
  <img src="png/font (93).png">
  <img src="png/font (94).png">
  <img src="png/font (95).png">
  <img src="png/font (96).png">
  <img src="png/font (97).png">
  <img src="png/font (98).png">
  <img src="png/font (99).png">
  <img src="png/font (100).png">
  <img src="png/font (101).png">
  <img src="png/font (102).png">
  <img src="png/font (103).png">
  <img src="png/font (104).png">
  <img src="png/font (105).png">
  <img src="png/font (106).png">
  <img src="png/font (107).png">
  <img src="png/font (108).png">
  <img src="png/font (109).png">
  <img src="png/font (110).png">
  <img src="png/font (111).png">
  <img src="png/font (112).png">
  <img src="png/font (113).png">
  <img src="png/font (114).png">
  <img src="png/font (115).png">
  <img src="png/font (116).png">
  <img src="png/font (117).png">
  <img src="png/font (118).png">
  <img src="png/font (119).png">
  <img src="png/font (120).png">
  <img src="png/font (121).png">
  <img src="png/font (122).png">
  <img src="png/font (123).png">
  <img src="png/font (124).png">
  <img src="png/font (125).png">
  <img src="png/font (126).png">
  <img src="png/font (127).png">
  <img src="png/font (128).png">
  <img src="png/font (129).png">
  <img src="png/font (130).png">
  <img src="png/font (131).png">
  <img src="png/font (132).png">
  <img src="png/font (133).png">
  <img src="png/font (134).png">
  <img src="png/font (135).png">
  <img src="png/font (136).png">
  <img src="png/font (137).png">
  <img src="png/font (138).png">
  <img src="png/font (139).png">
  <img src="png/font (140).png">
  <img src="png/font (141).png">
  <img src="png/font (142).png">
  <img src="png/font (143).png">
  <img src="png/font (144).png">
  <img src="png/font (145).png">
  <img src="png/font (146).png">
  <img src="png/font (147).png">
  <img src="png/font (148).png">
  <img src="png/font (149).png">
  <img src="png/font (150).png">
  <img src="png/letters (1).png">
  <img src="png/letters (2).png">
  <img src="png/letters (3).png">
  <img src="png/letters (4).png">
  <img src="png/letters (5).png">
  <img src="png/letters (6).png">
  <img src="png/letters (7).png">
  <img src="png/letters (8).png">
  <img src="png/letters (9).png">
  <img src="png/letters (10).png">
  <img src="png/letters (11).png">
  <img src="png/letters (12).png">
  <img src="png/letters (13).png">
  <img src="png/letters (14).png">
  <img src="png/letters (15).png">
  <img src="png/letters (16).png">
  <img src="png/letters (17).png">
  <img src="png/letters (18).png">
  <img src="png/letters (19).png">
  <img src="png/letters (20).png">
  <img src="png/letters (21).png">
  <img src="png/letters (22).png">
  <img src="png/letters (23).png">
  <img src="png/letters (24).png">
  <img src="png/letters (25).png">
</div>


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

