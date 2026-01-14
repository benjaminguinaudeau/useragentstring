# useragentstring

R package to scrape user agent strings from [useragentstring.com](https://useragentstring.com), including first and last visit dates.

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

## License

MIT
