# Licensing

Dactyl is two different kinds of work in one repository, so it uses two
licenses. Which one applies depends on *what* you are reusing.

## The fonts and the generator → SIL Open Font License 1.1

The generated font files (OTF/TTF) and the source that produces them are
licensed under the **SIL Open Font License, Version 1.1** — see
[`OFL.txt`](OFL.txt).

The OFL is the standard licence for libre/freeware fonts. In short, you may
use, study, modify, embed and redistribute the fonts freely — including in
commercial products and documents — provided that:

- you don't sell the fonts *on their own*;
- derivative fonts stay under the OFL and carry the copyright notice;
- derivatives don't use a Reserved Font Name (see below).

We deliberately chose the OFL over Creative Commons for the fonts because:

- CC licences were designed for creative works (images, prose), not for
  software or fonts, and map awkwardly onto font *embedding*.
- The OFL is what the font ecosystem expects. It is the entry requirement for
  **Google Fonts** and is understood by every major freeware distributor
  (dafont, FontSpace, Fontshare, …).
- A **NonCommercial** CC clause (as previously used, see below) would make the
  fonts ineligible for Google Fonts and most freeware sites, and leaves users
  unsure whether embedding in a commercial document is allowed.

### Reserved Font Name

`OFL.txt` currently declares **no Reserved Font Name**, which keeps the fonts
eligible for Google Fonts (Google refuses fonts that reserve a name).

If you would rather stop others from shipping modified fonts under the
"Dactyl" name, add a reserved-name line to the copyright block in `OFL.txt`:

```
Copyright 2020 The Dactyl Project Authors ..., with Reserved Font Name "Dactyl".
```

Note that doing so means the flagship family cannot be submitted to Google
Fonts under that name — the two goals are mutually exclusive, so decide per
distribution channel.

## The proofs and prose → Creative Commons BY-NC-SA 4.0

The proof texts under [`proofs/`](proofs/) remain under
**Creative Commons Attribution-NonCommercial-ShareAlike 4.0** — see
[`proofs/license.md`](proofs/license.md). These are sample writing used to
show the fonts, not part of the Font Software, so the NonCommercial clause
there does **not** restrict commercial use of the fonts themselves.

## Summary

| What you're reusing | Licence |
|---------------------|---------|
| Font files (OTF/TTF) | SIL OFL 1.1 |
| Font generator source (F#, JS build) | SIL OFL 1.1 |
| Proof texts in `proofs/` | CC BY-NC-SA 4.0 |
