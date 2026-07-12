import { test, expect } from '@playwright/test';

// Dedicated visual-regression coverage for the two-storey `alt_a_g`
// stylistic-alternate shapes.
//
// The tweens tab only renders the single character 'a', so the alternate 'g'
// would otherwise have no committed baseline.  This drives the Visual Diffs
// tab (alt_a_g off vs on, overlaid) over just the two affected letters "ag",
// so the snapshot changes only when the two-storey 'a' or 'g' change — never
// when unrelated glyphs are tweaked.
test.beforeEach(async ({ page }) => {
  // Clear localStorage so each run starts from deterministic defaults.
  await page.addInitScript(() => localStorage.clear());
  page.on('console', msg => {
    if (msg.type() === 'error') console.error('Browser error:', msg.text());
  });
  page.on('pageerror', err => console.error('Page error:', err.message));
});

test('alt_a_g two-storey a and g', async ({ page }) => {
  test.setTimeout(60_000);

  // Visual Diffs comparing alt_a_g off (red) vs on (blue), overlaid, via URL.
  await page.goto('/?view=visualDiffs&diffAxis=alt_a_g&diffA=0&diffB=1');

  // Focus the diff on just the two affected letters so unrelated glyph changes
  // never churn this baseline.
  const textarea = page.locator('.input-wrapper textarea');
  await textarea.waitFor({ timeout: 30_000 });
  await textarea.fill('ag');

  // Wait until the worker has re-rendered the small two-letter diff (the tab
  // initialises with the full character set, which has hundreds of paths).
  await page.waitForFunction(() => {
    const svg = document.querySelector('.svg-container svg');
    if (!svg) return false;
    const n = svg.querySelectorAll('path').length;
    return n > 0 && n < 40;
  }, { timeout: 45_000 });
  await page.waitForTimeout(200);

  // Hide the floating zoom controls so the snapshot doesn't depend on the
  // (network-loaded) icon font and stays a clean picture of just the glyphs.
  await page.addStyleTag({ content: '.zoom-controls { display: none !important; }' });

  await expect(page.locator('.svg-container')).toHaveScreenshot('alt-a-g.png');
});
