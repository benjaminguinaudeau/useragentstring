#' @importFrom httr2 request req_headers req_perform resp_body_html
#' @importFrom rvest html_elements html_attr html_text
#' @importFrom stringr str_detect str_extract str_starts str_trim
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom furrr future_map
#' @importFrom future plan multisession
#' @importFrom cli cli_alert cli_alert_info cli_alert_success
#' @importFrom tibble tibble
NULL

BASE_URL <- "https://useragentstring.com"

#' Get all user agent detail links for a specific browser
#'
#' @param browser Character string specifying the browser name (e.g., "Chrome", "Safari", "Opera")
#' @return Character vector of detail page URLs
#' @export
#' @examples
#' \dontrun{
#' links <- get_browser_links("Chrome")
#' head(links)
#' }
get_browser_links <- function(browser) {
  url <- paste0(BASE_URL, "/pages/", browser, "/")

  resp <- tryCatch({
    httr2::request(url) |>
      httr2::req_headers("User-Agent" = "R useragentstring package") |>
      httr2::req_perform()
  }, error = function(e) {
    cli::cli_alert_danger("Failed to fetch {browser} page: {e$message}")
    return(NULL)
  })


  if (is.null(resp)) return(character(0))

  html <- httr2::resp_body_html(resp)

  # Extract all links
  all_hrefs <- html |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")

  # Filter to detail page links (pattern: *_id_*.php)
  links <- all_hrefs[!is.na(all_hrefs) & stringr::str_detect(all_hrefs, "_id_\\d+\\.php$")]

  unique(links)
}

#' Scrape details from a single user agent page
#'
#' @param detail_url URL path to the detail page (e.g., "/Chrome134.0_id_20018.php")
#' @return A tibble with useragent_string, first_visit, last_visit, and detail_url
#' @export
#' @examples
#' \dontrun{
#' details <- get_useragent_details("/Chrome134.0.6998.166_id_20018.php")
#' }
get_useragent_details <- function(detail_url) {
  base_url <- "https://useragentstring.com"
  url <- if (stringr::str_starts(detail_url, "/")) {
    paste0(base_url, detail_url)
  } else {
    paste0(base_url, "/", detail_url)
  }

  resp <- tryCatch({
    httr2::request(url) |>
      httr2::req_headers("User-Agent" = "R useragentstring package") |>
      httr2::req_perform()
  }, error = function(e) NULL)

  if (is.null(resp)) {
    return(tibble::tibble(
      useragent_string = NA_character_,
      first_visit = NA_character_,
      last_visit = NA_character_,
      detail_url = detail_url
    ))
  }

  html <- httr2::resp_body_html(resp)
  text <- html |> rvest::html_text()

  # Extract user agent string - try multiple methods
  ua_string <- NA_character_


  # Method 1: Look for textarea element
  ua_string <- html |>
    rvest::html_element("textarea") |>
    rvest::html_text()

  # Method 2: Extract from page content - UA strings typically start with Mozilla/
  if (is.na(ua_string) || ua_string == "") {
    ua_string <- stringr::str_extract(text, "Mozilla/[^\n\r]+")
  }

  # Method 3: Look for other common UA patterns
  if (is.na(ua_string) || ua_string == "") {
    ua_string <- stringr::str_extract(text, "(?:Mozilla|Googlebot|curl|wget|python|Java|Apache|Opera)[^\\n\\r]{10,}")
  }

  # Clean up the UA string
  if (!is.na(ua_string)) {
    ua_string <- stringr::str_trim(ua_string)
  }

  # Extract first visit and last visit dates
  first_visit <- stringr::str_extract(text, "(?i)first\\s*visit[:\\s]+([0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\\s+[0-9]{2}:[0-9]{2})", group = 1)
  last_visit <- stringr::str_extract(text, "(?i)last\\s*visit[:\\s]+([0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\\s+[0-9]{2}:[0-9]{2})", group = 1)

  tibble::tibble(
    useragent_string = ua_string,
    first_visit = first_visit,
    last_visit = last_visit,
    detail_url = detail_url
  )
}

#' Scrape all user agents for a specific browser
#'
#' @param browser Character string specifying the browser name (e.g., "Chrome", "Safari", "Opera")
#' @param parallel Logical, whether to use parallel processing (default TRUE)
#' @param workers Number of parallel workers (default 4)
#' @param limit Optional limit on number of user agents to scrape (useful for testing)
#' @return A tibble with columns: browser, version, useragent_string, first_visit, last_visit, detail_url
#' @export
#' @examples
#' \dontrun{
#' # Scrape first 10 Chrome user agents
#' chrome_sample <- scrape_browser("Chrome", limit = 10)
#'
#' # Scrape all Chrome user agents with parallelization
#' chrome_all <- scrape_browser("Chrome", parallel = TRUE, workers = 4)
#' }
scrape_browser <- function(browser, parallel = TRUE, workers = 4, limit = NULL) {
  cli::cli_alert_info("Fetching {browser} user agent links...")
  links <- get_browser_links(browser)

  if (length(links) == 0) {
    cli::cli_alert_danger("No links found for {browser}")
    return(tibble::tibble())
  }

  cli::cli_alert_success("Found {length(links)} user agent pages for {browser}")

  # Apply limit if specified

  if (!is.null(limit) && limit < length(links)) {
    links <- links[1:limit]
    cli::cli_alert_info("Limited to first {limit} entries")
  }

  cli::cli_alert_info("Scraping user agent details...")

  if (parallel && length(links) > 10) {
    # Use parallel processing
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)

    results <- furrr::future_map(
      links,
      get_useragent_details,
      .progress = TRUE,
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    # Sequential processing
    results <- purrr::map(links, get_useragent_details, .progress = TRUE)
  }

  # Combine results
  results_df <- dplyr::bind_rows(results)

  # Add browser name and extract version
  results_df <- results_df |>
    dplyr::mutate(
      browser = browser,
      version = stringr::str_extract(detail_url, "([0-9]+\\.?[0-9]*\\.?[0-9]*\\.?[0-9]*)_id_", group = 1),
      first_visit = as.POSIXct(first_visit, format = "%Y.%m.%d %H:%M"),
      last_visit = as.POSIXct(last_visit, format = "%Y.%m.%d %H:%M")
    ) |>
    dplyr::select(browser, version, useragent_string, first_visit, last_visit, detail_url)

  cli::cli_alert_success("Scraped {nrow(results_df)} user agents for {browser}")

  results_df
}

#' Get all available browser/crawler categories from useragentstring.com
#'
#' @return A tibble with columns: name (browser/crawler name), category (CRAWLERS, BROWSERS, etc.)
#' @export
#' @examples
#' \dontrun{
#' categories <- get_all_categories()
#' head(categories)
#' }
get_all_categories <- function() {
  base_url <- "https://useragentstring.com"
  url <- paste0(base_url, "/pages/useragentstring.php")

  resp <- httr2::request(url) |>
    httr2::req_headers("User-Agent" = "R useragentstring package") |>
    httr2::req_perform()

  html <- httr2::resp_body_html(resp)

  # Get all links in document order
  all_a_tags <- html |> rvest::html_elements("a")
  all_hrefs <- all_a_tags |> rvest::html_attr("href")
  all_texts <- all_a_tags |> rvest::html_text()
  all_classes <- all_a_tags |> rvest::html_attr("class")

  # Build a data frame preserving document order
  all_links <- tibble::tibble(
    idx = seq_along(all_hrefs),
    href = all_hrefs,
    text = stringr::str_trim(all_texts),
    class = all_classes
  )

  # Identify category headers
  cat_rows <- all_links |>
    dplyr::filter(.data$class == "unterMenuTitel") |>
    dplyr::select(cat_idx = "idx", category = "text")

  # Identify browser/crawler links
  browser_rows <- all_links |>
    dplyr::filter(
      !is.na(.data$href),
      stringr::str_detect(.data$href, "^/pages/.+/"),
      is.na(.data$class) | .data$class != "unterMenuTitel",
      !.data$text %in% c("Home", "List of User Agent Strings", "Links", "", "ALL"),
      !stringr::str_detect(.data$href, "Crawlerlist|Browserlist|unknown|useragentstring\\.php|links\\.php")
    )

  # Assign category based on element order (find most recent category header before each link)
  browser_rows <- browser_rows |>
    dplyr::mutate(
      category = purrr::map_chr(.data$idx, function(i) {
        valid_cats <- cat_rows$cat_idx[cat_rows$cat_idx < i]
        if (length(valid_cats) == 0) return("Unknown")
        cat_rows$category[cat_rows$cat_idx == max(valid_cats)]
      })
    ) |>
    dplyr::select(name = "text", "category") |>
    dplyr::distinct()

  browser_rows
}
