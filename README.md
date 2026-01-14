# useragentstring

<!-- badges: start -->
[![Update Data](https://github.com/benjaminguinaudeau/useragentstring/actions/workflows/update-data.yml/badge.svg)](https://github.com/benjaminguinaudeau/useragentstring/actions/workflows/update-data.yml)
<!-- badges: end -->

R package to scrape user agent strings from [useragentstring.com](https://useragentstring.com), including first and last visit dates.

**[View Documentation & EDA](https://benjaminguinaudeau.github.io/useragentstring/)**

## Installation

```r
# Install from GitHub
devtools::install_github("benjaminguinaudeau/useragentstring")
```
## Usage

### Scrape user agents for a specific browser

```r
library(useragentstring)

# Scrape all Chrome user agents (with parallel processing)
chrome_ua <- scrape_browser("Chrome")

# Scrape with limited entries (useful for testing)
chrome_sample <- scrape_browser("Chrome", limit = 100)

# Scrape without parallelization
chrome_seq <- scrape_browser("Chrome", parallel = FALSE)

# Scrape other browsers
safari_ua <- scrape_browser("Safari")
opera_ua <- scrape_browser("Opera")
```

### Output format

The `scrape_browser()` function returns a tibble with:

| Column | Description |
|--------|-------------|
| `browser` | Browser name (e.g., "Chrome") |
| `version` | Version number |
| `useragent_string` | Full user agent string |
| `first_visit` | First time this UA was seen (POSIXct) |
| `last_visit` | Last time this UA was seen (POSIXct) |
| `detail_url` | Source URL on useragentstring.com |

### Lower-level functions

```r
# Get all detail page links for a browser
links <- get_browser_links("Chrome")

# Scrape a single detail page
details <- get_useragent_details(links[1])
```

## Parallelization

By default, `scrape_browser()` uses parallel processing via the `furrr` package for faster scraping. You can control this with:

```r
# Use 8 workers
chrome_ua <- scrape_browser("Chrome", workers = 8)

# Disable parallelization
chrome_ua <- scrape_browser("Chrome", parallel = FALSE)
```

## Automated Data Updates

The user agent data is automatically updated **every Sunday at midnight UTC** via GitHub Actions. The workflow:

1. Scrapes all major browsers (Chrome, Firefox, Safari, Opera, Edge, etc.)
2. Scrapes major crawlers (Googlebot, Bingbot, YandexBot, etc.)
3. Rebuilds the package website with updated EDA
4. Commits and pushes changes

You can also manually trigger an update from the [Actions tab](https://github.com/benjaminguinaudeau/useragentstring/actions/workflows/update-data.yml).

## Pre-scraped Data

The package includes pre-scraped datasets in `data/` folder:
- `chrome.rds`, `firefox.rds`, `safari.rds`, `opera.rds`, `edge.rds`
- `googlebot.rds`, `bingbot.rds`, `yandexbot.rds`, etc.

Combined CSV available at `user_agents.csv`.

## License

MIT
