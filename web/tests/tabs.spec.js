import { test, expect } from '@playwright/test';

// The tabs and the CSS selector to wait on before screenshotting.
// `url` overrides the default `?view={name}` navigation (used for the proofs
// variants where both the tab and the case selector are encoded in the URL).
const TABS = [
  { name: 'font',             selector: '.svg-container' },
  { name: 'glyphs',           selector: '.svg-container' },
  { name: 'tweens',           selector: '.tweens-grid'   },
  { name: 'visualDiffs',      selector: '.svg-container' },
  { name: 'splines',          selector: '.se-canvas'     },
  { name: 'splineGrid',       selector: '.sg-grid'       },
  { name: 'proofs-uppercase', selector: '.svg-container',
    url: '?view=proofs&proof=uppercase' },
  { name: 'proofs-lowercase', selector: '.svg-container',
    url: '?view=proofs&proof=lowercase' },
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

for (const { name, selector, url } of TABS) {
  test(`${name} tab`, async ({ page }) => {
    await page.goto(url ?? `?view=${name}`);

    // Wait for the Web Worker to finish rendering (up to 2 min for tweens)
    await page.waitForSelector(selector, { timeout: 90_000 });

    // Screenshot just the preview area (excludes the axis sidebar)
    await expect(page.locator('.preview-content')).toHaveScreenshot(`${name}.png`);
  });
}
