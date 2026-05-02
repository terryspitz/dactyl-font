import { test, expect } from '@playwright/test';

// The tabs and the CSS selector to wait on before screenshotting.
// `url` overrides the default `?view={name}` navigation (used for the proofs
// variants where both the tab and the case selector are encoded in the URL).
// `extraWait` is an optional async function called after the selector appears,
// for tabs that need additional settling (e.g. font-face loading).
// `tallViewport` temporarily enlarges the viewport height so the full content
// fits without scrolling (used for tabs whose content overflows 900px).
const TABS = [
  { name: 'font',             selector: '.svg-container' },
  { name: 'glyphs',           selector: '.svg-container' },
  { name: 'tweens',           selector: '.tweens-grid'   },
  { name: 'visualDiffs',      selector: '.svg-container' },
  { name: 'splines',          selector: '.se-canvas'     },
  { name: 'splineGrid',       selector: '.sg-grid',       tallViewport: true },
  { name: 'proofs-uppercase', selector: '.proof-text',
    url: '?view=proofs&proof=uppercase',
    extraWait: async (page) => {
      // Wait for the @font-face data URL to be injected then the font to load
      await page.waitForFunction(() => !!document.getElementById('dactyl-proof-font'), { timeout: 90_000 });
      await page.evaluate(() => document.fonts.ready);
    },
  },
  { name: 'proofs-lowercase', selector: '.proof-text',
    url: '?view=proofs&proof=lowercase',
    extraWait: async (page) => {
      await page.waitForFunction(() => !!document.getElementById('dactyl-proof-font'), { timeout: 90_000 });
      await page.evaluate(() => document.fonts.ready);
    },
  },
  { name: 'proofs-classic', selector: '.proof-text',
    url: '?view=proofs&proof=classic&book=0',
    extraWait: async (page) => {
      await page.waitForFunction(() => !!document.getElementById('dactyl-proof-font'), { timeout: 90_000 });
      await page.evaluate(() => document.fonts.ready);
    },
  },
];

test.beforeEach(async ({ page }) => {
  // Clear localStorage so each test starts from deterministic defaults
  await page.addInitScript(() => localStorage.clear());

  // Surface any browser console errors in the test output
  page.on('console', msg => {
    if (msg.type() === 'error') console.error('Browser error:', msg.text());
  });
  page.on('pageerror', err => console.error('Page error:', err.message));
});

for (const { name, selector, url, extraWait, tallViewport } of TABS) {
  test(`${name} tab`, async ({ page }) => {
    await page.goto(url ?? `?view=${name}`);

    // Wait for the Web Worker to finish rendering (up to 2 min for tweens)
    await page.waitForSelector(selector, { timeout: 90_000 });

    // Per-tab extra settling (e.g. font-face loading for proofs)
    if (extraWait) await extraWait(page);

    if (tallViewport) {
      // Expand the viewport so the flex layout makes .preview-content tall enough
      // to show all content without internal scrolling (splineGrid is ~2700px tall).
      await page.setViewportSize({ width: 1280, height: 3000 });
    }

    // Screenshot just the preview area (excludes the axis sidebar)
    await expect(page.locator('.preview-content')).toHaveScreenshot(`${name}.png`);
  });
}
