import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: './tests',
  timeout: 120_000,       // per-test timeout (tweens tab is slow)
  use: {
    baseURL: 'http://localhost:4173/dactyl-font/',
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
  webServer: {
    command: 'npm run preview',
    url: 'http://localhost:4173/dactyl-font/',
    reuseExistingServer: !process.env.CI,
    timeout: 30000,
  },
});
