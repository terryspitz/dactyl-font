// @ts-check
import { test, expect } from '@playwright/test';

// All range-type axes (excludes checkboxes), minus tracking/leading which the app excludes
const TWEEN_AXES = [
  'width', 'height', 'x_height', 'thickness', 'contrast',
  'roundedness', 'soft_corners', 'monospace', 'italic',
  'serif', 'end_bulb', 'flare', 'max_spline_iter', 'flatness',
];

// Wait for the tweens grid to be fully rendered (no loading spinner, grid populated)
async function waitForTweens(page) {
  // Wait for at least one tween row with SVG content
  await page.waitForSelector('.tween-row svg', { timeout: 60_000 });
  // Give a short extra pause for any remaining renders
  await page.waitForTimeout(500);
}

test.describe('Tweens visual tests', () => {
  // Screenshot all tweens together (default view)
  test('all tweens', async ({ page }) => {
    await page.goto('/?view=tweens');
    await waitForTweens(page);
    const grid = page.locator('.tweens-grid');
    await grid.screenshot({ path: `screenshots/tweens-all.png` });
  });

  // Screenshot each axis separately using the ?tween= URL param
  for (const axis of TWEEN_AXES) {
    test(`tween ${axis}`, async ({ page }) => {
      await page.goto(`/?view=tweens&tween=${axis}`);
      await waitForTweens(page);
      const grid = page.locator('.tweens-grid');
      await grid.screenshot({ path: `screenshots/tween-${axis}.png` });
    });
  }
});
