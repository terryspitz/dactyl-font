import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: './tests',
  timeout: 120_000,       // per-test timeout (tweens tab is slow)
  expect: { timeout: 30_000 },
  use: {
    baseURL: process.env.PLAYWRIGHT_BASE_URL || 'http://localhost:4173/dactyl-font/',
    viewport: { width: 1280, height: 900 },
    screenshot: 'only-on-failure',
    trace: 'on-first-retry',
  },
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],
  ...(process.env.PLAYWRIGHT_BASE_URL ? {} : {
    webServer: {
      command: 'npm run preview',
      url: 'http://localhost:4173/dactyl-font/',
      reuseExistingServer: !process.env.CI,
      timeout: 30000,
    },
  }),
});
