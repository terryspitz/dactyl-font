// @ts-check
import { test, expect } from '@playwright/test';
import { readFileSync } from 'fs';

test.beforeEach(async ({ page }) => {
  await page.addInitScript(() => localStorage.clear());
  page.on('pageerror', err => console.error('Page error:', err.message));
});

test('download font renders text correctly', async ({ page }) => {
  await page.goto('?view=font');

  // Wait for the font tab SVG to be ready before clicking download
  await page.waitForSelector('.svg-container', { timeout: 90_000 });

  // Click the download button and capture the file
  const downloadPromise = page.waitForEvent('download', { timeout: 120_000 });
  await page.click('[title="Download Font (OTF)"]');
  const download = await downloadPromise;

  // Save to the test output directory
  const fontPath = test.info().outputPath('dactyl.otf');
  await download.saveAs(fontPath);

  // Embed as a base64 data URL so we don't need a server to serve it
  const base64 = readFileSync(fontPath).toString('base64');

  // Render a standalone page using the downloaded font
  await page.setContent(`<!DOCTYPE html>
    <html>
      <head>
        <style>
          @font-face {
            font-family: 'DactylDownload';
            src: url('data:font/otf;base64,${base64}') format('opentype');
          }
          body {
            background: white;
            padding: 40px;
            margin: 0;
          }
          p {
            font-family: 'DactylDownload', monospace;
            font-size: 60px;
            line-height: 1.4;
            color: black;
            margin: 0;
          }
        </style>
      </head>
      <body>
        <p>ABCDEFGHIJKLM</p>
        <p>NOPQRSTUVWXYZ</p>
        <p>abcdefghijklm</p>
        <p>nopqrstuvwxyz</p>
        <p>0123456789</p>
      </body>
    </html>`);

  // Explicitly load the font face before screenshotting
  await page.evaluate(() => document.fonts.load("60px 'DactylDownload'"));
  await page.evaluate(() => document.fonts.ready);

  await expect(page.locator('body')).toHaveScreenshot('font-download-render.png');
});
