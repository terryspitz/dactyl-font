import { test, expect } from '@playwright/test';

// The 5 main tabs and the CSS selector to wait on before screenshotting
const TABS = [
  { name: 'font',        selector: '.svg-container' },
  { name: 'glyphs',      selector: '.svg-container' },
  { name: 'tweens',      selector: '.tweens-grid'   },
  { name: 'visualDiffs', selector: '.svg-container' },
  { name: 'splines',     selector: '.se-canvas'     },
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

for (const { name: tab, selector } of TABS) {
  test(`${tab} tab`, async ({ page }) => {
    await page.goto(`?view=${tab}`);

    // Wait for the Web Worker to finish rendering (up to 2 min for tweens)
    await page.waitForSelector(selector, { timeout: 90_000 });

    // Screenshot just the preview area (excludes the axis sidebar)
    await expect(page.locator('.preview-content')).toHaveScreenshot(`${tab}.png`);
  });
}
