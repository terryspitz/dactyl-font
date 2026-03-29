import { test, expect } from '@playwright/test';

// The 5 main tabs and the CSS selector to wait on before screenshotting
const TABS = [
  { name: 'font',        selector: '.svg-container' },
  { name: 'glyphs',      selector: '.svg-container' },
  { name: 'tweens',      selector: '.tweens-grid'   },
  { name: 'visualDiffs', selector: '.svg-container' },
  { name: 'splines',     selector: '.svg-container' },
];

test.beforeEach(async ({ page }) => {
  // Clear localStorage so each test starts from deterministic defaults
  await page.addInitScript(() => localStorage.clear());
});

for (const { name: tab, selector } of TABS) {
  test(`${tab} tab`, async ({ page }) => {
    await page.goto(`?view=${tab}`);

    // Wait for the Web Worker to finish rendering
    await page.waitForSelector(selector, { timeout: 60_000 });

    // Screenshot just the preview area (excludes the axis sidebar)
    await expect(page.locator('.preview-content')).toHaveScreenshot(`${tab}.png`);
  });
}
