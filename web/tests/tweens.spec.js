// @ts-check
import { test, expect } from '@playwright/test';

// Wait for the tweens grid to be fully rendered
async function waitForTweens(page) {
  await page.waitForSelector('.tween-row svg', { timeout: 60_000 });
  await page.waitForTimeout(500);
}

test.describe('Tweens visual tests', () => {
  // Screenshot all tweens together (default view)
  test('all tweens', async ({ page }) => {
    await page.goto('/?view=tweens');
    await waitForTweens(page);
    const grid = page.locator('.tweens-grid');
    await grid.screenshot({ path: 'screenshots/tweens-all.png' });
  });

  // Screenshot each axis separately, discovered from the rendered page
  test('individual tweens', async ({ page }) => {
    // Load all tweens to discover which axes the app generates
    await page.goto('/?view=tweens');
    await waitForTweens(page);
    const axes = await page.$$eval('.tween-row h4', els => els.map(el => el.textContent));

    for (const axis of axes) {
      await page.goto(`/?view=tweens&tween=${axis}`);
      await waitForTweens(page);
      const grid = page.locator('.tweens-grid');
      await grid.screenshot({ path: `screenshots/tween-${axis}.png` });
    }
  });
});
