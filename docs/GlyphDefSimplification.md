# Simplifying the glyph definition strings

A survey of `src/generator/GlyphStringDefs.fs` (`glyphMap`) looking for
repetition that the language could factor out, and a ranked set of proposals.
Nothing here changes any rendered outline — every rewrite shown is intended to
parse to exactly the same knots as today.

**The table today:** 104 glyph definitions, 162 strokes, 2291 characters.

---

## 1. What the survey found

### 1.1 Strokes that are already byte-identical across glyphs

| Repeats | Stroke | Glyphs |
|--------:|--------|--------|
| 5 | `xor~x(c)~(xb)l~b(c)~bor` | `a c d g q` |
| 5 | `tl-bl` | `b H h K k` |
| 4 | `xl-bl` | `i m n r` |
| 4 | `hl-hr` | `+ - E H` |
| 3 | `(h)l~t(c)~(h)r~b(c)~` | `0 O Q` |
| 3 | `tel-1/3thlc` | `‘` `“` and the backtick |
| 2 | `thr~t(c)~(1/3tb)l~hc~(2/3tb)r~b(c)~bhl` | `$ S` |
| 2 | `bol~b(c)~(xb)r~x(c)~xol` | `b p` |
| 2 | `xol~x(c)~xbr-br` | `h n` |
| 2 | `tec-bec` | `$ \|` |

326 characters of the table are literally re-typed copies. The cost isn't the
characters, it's that **the lowercase bowl exists in seven places**: tuning `c`'s
aperture means finding and editing `a d g q` (and mirroring into `b p`) by hand.

Two whole-glyph inclusions are worth calling out:

* `$` = `S` + `tec-bec`. The `S` string is duplicated verbatim.
* `B` = `hl-hlo~(bh)r~blo-` + **the entire `P` definition**.

### 1.2 Six case pairs are the same skeleton in a different box

Substituting `t`→`x` (and `h`→`xb`, the middle of the new box) in the capital
gives the lowercase *exactly*:

```
C  tor~t(c)~(h)l~b(c)~bor      →  c  xor~x(c)~(xb)l~b(c)~bor   ✓
O  (h)l~t(c)~(h)r~b(c)~        →  o  (xb)l~x(c)~(xb)r~b(c)~    ✓
V  tl-bc-tr                    →  v  xl-bc-xr                  ✓
W  tl-b1/4lw-tlw-b3/4lw-tw     →  w  xl-b1/4lw-xlw-b3/4lw-xw   ✓
X  tl-br tr-bl                 →  x  xl-br xr-bl               ✓
Z  tl-tr-bl-br                 →  z  xl-xr-bl-br               ✓
```

This is the "which edges does it touch" instinct made concrete: these strokes
don't care about cap height or x-height, only about *the top and bottom of their
own box*. Say which box, and the shape is written once.

`S`/`s` are a near miss, and the diff is instructive — it's idiom drift, not design:

```
S  thr~t(c)~(1/3tb)l~hc  ~(2/3tb)r~b(c)~bhl
s  xor~x(c)~(1/3xb)l~xbcE~(2/3xb)r~b(c)~bol
```

The bowl is the same; the *terminals* are specified two different ways
(`thr`/`bhl` — a height three-quarters up — versus `xor`/`bol` — the guide pulled
back by roundedness), and `s` pins its waist with an explicit `E` while `S`
leaves it free. One of these is better than the other and both letters should use
it.

### 1.3 Pairs related by a transform

* `9` is **exactly** `6` rotated 180° (`t`↔`b`, `l`↔`r`, `N`↔`S`):
  `6 = tor~t(c)~(h)l~1/3btl~b(c)~1/3btr~1/3tbc~1/3btlNJ`,
  `9 = bol~b(c)~(h)r~1/3tbr~t(c)~1/3tbl~1/3btc~1/3tbrSJ`.
* `b`/`p`'s bowl is `c`'s bowl mirrored in x (and written in the opposite drawing
  direction, which is why the strings don't match textually).
* `“` = `‘` + a copy shifted right; `”` = `’` + a copy shifted right; `"`, `#`,
  `=`, `:` are all "one stroke, twice, offset".
* `B` is one lobe in the box `[t..h]` plus the same lobe in `[h..b]`; `D` is that
  lobe over the full height; `P` is it plus a stem; `R` is `P` plus a leg. Four
  capitals, one shape.

### 1.4 Idiom inconsistencies found on the way

These need no new syntax and are worth fixing regardless:

* `S`/`s` terminals and waist, as above.
* `M` is not the vertical flip of `W`: `M = bl-tl-blw-tw-bw` puts its feet on the
  corners and leaves its left side upright while the right side splays, whereas
  `W = tl-b1/4lw-tlw-b3/4lw-tw` insets both feet to a quarter and three
  quarters of its width. Deliberate or not,
  the two letters are currently designed by different rules.
* Bowl terminals are variously `tor`, `xor`, `thr`, `bol`, `bhr` — three
  different ways of saying "cut the bowl open near the right".
* Drawing direction is inconsistent (`B` starts at the waist and runs down, `P`
  starts at the foot and runs up), which hides sharing that is really there.

---

## 2. Proposals, in payoff order

### A. A named stroke library — `$name`

A second map of named fragments, textually expanded before parsing:

```fsharp
let strokeMap =
    Map.ofList
        [ "bowl", "tor~t(c)~(h)l~b(c)~bor"   // C, and the bowl of a c d g q
          "lobe", "tl-tlo~(tb)r~blo-bl"      // the flat-shouldered bowl of B D P R
          "arch", "xol~x(c)~xbr-br" ]        // the shoulder of h m n
```

```
'c', "$bowl"      'h', "tl-bl $arch"
'a', "xr-br $bowl"    'n', "xl-bl $arch"
'd', "tr-br $bowl"    '$', "$S tec-bec"
```

*Buys:* one place to tune each recurring shape. *Cost:* ~20 lines (a regex
expansion in `stringDefsToElem`, plus the same expansion on `rawDefToElem` so the
Glyphs-tab editor accepts `$bowl` too). *Risk:* none to geometry — pure string
substitution.

### B. A per-stroke box — `{x}` / `{t,h}`

Prefix a stroke with the guides its box spans; inside that stroke `t` and `b`
mean the box's own top and bottom (and `h` its middle). Default is `{t,b}`, so
every existing definition keeps working unchanged.

```
'C', "$bowl"          'O', "$ring"        'B', "{h,b}$lobe {t,h}$lobe"
'c', "{x}$bowl"       'o', "{x}$ring"     'P', "bl-{t,h}$lobe"
```

*Buys:* the six exact case pairs above collapse to one definition each; `S`/`s`
join them once their terminals agree; `B D P R` become one lobe used four times;
`x`-height and cap versions of anything stay in sync automatically. This is the
direct expression of "define the glyph by the edges it touches".

*Cost:* moderate — a box parameter threaded into `parse_point`'s coordinate
lookup. Note it interacts with two optical corrections that key off the *real*
guides: `balanceRaise` (which must still ask "is this a mid height?" in box
terms) and `applyOvershoot` (which tests `onGuide` against real T/X/B/D — a
`{x,b}` box's top *is* a real guide, so this mostly falls out, but it needs a
test).

*Aside:* `S`'s `th` is 75% of the cap box, and until fractions landed (D below)
there was no way to write "75% of the x box" — `xxb` is 67% — which is part of
why `s` drifted to a different idiom. `3/4xb` now says it, so this no longer
blocks unifying the pair.

### C. Transforms on a stroke reference

Single-character prefixes: `|` mirror in x about the glyph centre, `%` rotate
180°, `_` flip in y within the box, `+c` repeat translated.

```
'9', "%$6"        'b', "tl-bl |{x}$bowl"      '“', "$tick +c$tick"
'w', "{x}$W"      'p', "xl-dl |{x}$bowl"      '”', "|$tick +c|$tick"
```

*Buys:* the mirrored bowls (`b p` vs `a c d g q`), `6`/`9`, the quote family,
`"` `#` `=` `:`. *Cost:* small if done on the parsed knot list rather than on the
string (mirroring text would have to swap `l`↔`r`, `N`↔`S`, `E`↔`W` *and* reverse
the point order — doable but fiddly; transforming coordinates after parse is
cleaner and also reverses drawing direction correctly).

### D. Fractions instead of letter repeats — **done**

*Was:* `bl3w`, `x2bc3l`, `h8tl4r`, `ttbl`, `bbtrcc` were the least readable part
of the table — you had to count repeated letters (or expand a digit) to see what
proportion was meant — and `K`'s definition needed a five-line comment to explain
that `h8tl4r` starts the leg a fifth of the way along the arm.

*Now:* a coordinate can be written as **`n/dAB`** — `n/d` of the way from guide
`A` to guide `B`: `b1/4lw`, `1/3xb1/4cl`, `1/9ht1/5lr`, `1/3tbl`, `1/3bt2/3rc`.
Averaging several letters still means what it did (`bt` is halfway, `llcr` an
off-grid point), so the fraction is used wherever the average would need more
than two letters. The digit-repeat shorthand (`b2t`, `r4c`) is gone: it existed
only to abbreviate long repeat runs, which the fraction now states outright.

Internally a fraction expands to exactly the average it names — `1/4lw` to three
parts `l` and one part `w`, in that order — so every glyph renders identically
(verified: byte-identical SVG for all 105 glyphs across eight axis settings).

### E. Points expressed on another stroke — for joints

Every awkward definition in the table is a three-way junction where one stroke
has to *find* a point on another: `A`'s crossbar, `K`'s and `k`'s legs, `m`'s
second arch, `Y`, `R`. They are currently hand-computed grid coordinates plus a
paragraph of comment explaining why that coordinate is the right one, and they
break whenever the host stroke moves.

A reference like `@arm:1/5` ("a fifth along the stroke named `arm`") would make
`K = tl-bl  tr-1/10hblJ  @arm:1/5J-br` say what the comment says, and stay
correct if the arm is redrawn. This is the largest change of the six and the one with the
most design leverage; worth prototyping on `K k m A Y` alone.

### F. Aperture by angle (speculative)

Bowl terminals (`C c S s G a e`) are all "cut the ring open near here", written
today as a guide plus an `o` offset. `$ring^30` ("open 30° at each end") would
say it once and make aperture a tunable axis rather than a hand-placed point.
Only worth doing if aperture consistency across `C c G S s e` turns out to matter
visually.

---

## 3. What the first three would look like

Applying A + B + C to the 37 glyphs where they clearly apply
(`O o 0 Q C c a d q g b p G V v W w X x Z z P D B R n h 6 9 S $ s`, the quote
family and the backtick):

* now: 972 characters across those definitions
* proposed: 558 characters, including an 11-entry stroke library
* **43% smaller, and about 40% of the table's characters stop being duplicates**

The real win is the maintenance one: the lowercase bowl goes from 7 copies to 1,
`B D P R` from 4 hand-drawn bowls to 1, and each case pair from 2 skeletons to 1.

---

## 4. Glyphs that should stay explicit

Not everything wants factoring. These carry genuinely individual geometry —
kinks, explicit tangents, interior joints, or shapes with no relatives — and
should keep their hand-written definitions:

`@ % & e f t g m u y 2 3 5 8 k K` and the punctuation one-offs.

The `{box}` prefix and `$name` references are opt-in, so these are simply left
alone.

---

## 5. Doing it safely

1. **Expand before parse.** `stringDefsToElem`, `rawDefToElem` (the Glyphs-tab
   live editor) and `Api.getGlyphDefs` / `getGlyphList` (which surface raw def
   strings to the UI) all need the expansion, or the editor will reject `$bowl`.
   Consider showing both the source and the expanded form in the Glyphs tab.
2. **Prove it's a no-op.** Add a test that parses every glyph from the old table
   and from the new one and asserts identical knot lists (position, type,
   tangents, joint flags). A rewrite that passes that test cannot move a
   snapshot — which matters here, since visual baselines are rebaselined by hand,
   not by CI.
3. **Sequence it.** D (fractions) is done. A (library) is independently useful
   and lands in an afternoon. B (box) is the one that pays for itself. C
   (transforms) is best done on knots, after B. E and F are separate projects;
   E is the interesting one.
