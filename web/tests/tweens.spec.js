// @ts-check
import { test, expect } from '@playwright/test';

// Wait for the tweens grid to be fully rendered
async function waitForTweens(page) {
  await page.waitForSelector('.tween-row svg', { timeout: 60_000 });
  await page.waitForTimeout(200);
}

test.describe('Tweens visual tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.addInitScript(() => localStorage.clear());
    page.on('console', msg => {
      if (msg.type() === 'error') console.error('Browser error:', msg.text());
    });
    page.on('pageerror', err => console.error('Page error:', err.message));
  });

  // Snapshot each axis separately, discovered from the rendered page.
  // The worker computes ALL tween data once on the initial load; each axis
  // switch is a cheap React re-render via popstate — no full page reload needed.
  test('individual tweens', async ({ page }) => {
    test.setTimeout(120_000);
    // Load all tweens once to let the worker finish computing all axes
    await page.goto('/?view=tweens&zoom=0.85');
    await waitForTweens(page);
    const axes = await page.$$eval('.tween-row h4', els => els.map(el => el.textContent));

    for (const axis of axes) {
      // Switch the tween filter via pushState+popstate — the app's popstate
      // listener updates tweenFilter state so React re-renders without a page reload.
      await page.evaluate((a) => {
        const url = new URL(window.location);
        url.searchParams.set('tween', a);
        window.history.pushState({}, '', url);
        window.dispatchEvent(new PopStateEvent('popstate'));
      }, axis);
      await waitForTweens(page);
      await expect(page.locator('.tweens-grid')).toHaveScreenshot(`tween-${axis}.png`);
    }
  });
});
