#' @importFrom httr2 request req_headers req_perform req_timeout resp_status resp_body_string
#' @importFrom stringr str_detect regex
#' @importFrom dplyr mutate summarise group_by bind_rows case_when n
#' @importFrom purrr map map_dfr safely
#' @importFrom tibble tibble
#' @importFrom cli cli_alert_info cli_progress_bar cli_progress_update cli_progress_done
NULL

#' Default test sites for blocking analysis
#'
#' Returns a character vector of URLs used for testing user agent blocking rates.
#' Sites are ordered by expected strictness level.
#'
#' @return Character vector of test URLs
#' @export
#' @examples
#' sites <- default_test_sites()
#' print(sites)
default_test_sites <- function() {
  c(
    # Tier 1: Baseline (always accepts)
    "https://httpbin.org/user-agent",
    # Tier 2: Standard protection
    "https://www.google.com/",
    "https://www.bing.com/",
    "https://www.wikipedia.org/",
    # Tier 3: Moderate protection
    "https://www.amazon.com/",
    "https://www.reddit.com/",
    # Tier 4: Strict protection
    "https://www.linkedin.com/",
    "https://www.cloudflare.com/",
    "https://www.instagram.com/"
  )
}

#' Blocked status codes
#' @keywords internal
blocked_status_codes <- function() {
  c(403L, 429L, 503L, 406L, 401L, 407L)
}

#' Blocked content patterns
#' @keywords internal
blocked_content_patterns <- function() {
  c(
    "captcha",
    "challenge",
    "access denied",
    "blocked",
    "unusual traffic",
    "verify you are human",
    "security check",
    "rate limit",
    "too many requests",
    "suspicious activity",
    "please enable javascript",
    "enable cookies"
  )
}

#' Detect if a response indicates blocking
#'
#' Analyzes HTTP response characteristics to determine if the request was blocked.
#'
#' @param status_code HTTP status code from response
#' @param content Response body content as string
#' @param response_time Time taken for the response in seconds
#' @return A list with is_blocked (logical), reason (character), and score (numeric 0-1)
#' @export
#' @examples
#' result <- detect_blocking(403, "Access Denied", 0.5)
#' print(result$is_blocked)  # TRUE
detect_blocking <- function(status_code, content, response_time) {
  # Check status code

if (status_code %in% blocked_status_codes()) {
    return(list(
      is_blocked = TRUE,
      reason = paste0("HTTP ", status_code),
      score = 0
    ))
  }

  # Check for blocking patterns in content
  content_lower <- tolower(content)
  patterns <- blocked_content_patterns()

  for (pattern in patterns) {
    if (stringr::str_detect(content_lower, stringr::regex(pattern, ignore_case = TRUE))) {
      return(list(
        is_blocked = TRUE,
        reason = paste0("Content pattern: ", pattern),
        score = 0.25
      ))
    }
  }

  # Check for suspicious response characteristics
  if (response_time > 10) {
    return(list(
      is_blocked = FALSE,
      reason = "Slow response (possible challenge)",
      score = 0.5
    ))
  }

  if (nchar(content) < 100 && response_time < 0.05) {
    return(list(
      is_blocked = FALSE,
      reason = "Suspiciously small response",
      score = 0.5
    ))
  }

  # Success
  list(
    is_blocked = FALSE,
    reason = "OK",
    score = 1
  )
}

#' Test a single user agent against a URL
#'
#' Makes an HTTP request to the specified URL using the given user agent string
#' and analyzes the response for blocking indicators.
#'
#' @param url Target URL to test
#' @param user_agent User agent string to use for the request
#' @param timeout Request timeout in seconds (default 15)
#' @return A tibble with columns: url, user_agent, status_code, response_time, is_blocked, reason, score
#' @export
#' @examples
#' \dontrun{
#' result <- test_user_agent(
#'   "https://httpbin.org/user-agent",
#'   "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0.0.0"
#' )
#' }
test_user_agent <- function(url, user_agent, timeout = 15) {
  start_time <- Sys.time()

  result <- tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_headers("User-Agent" = user_agent) |>
      httr2::req_headers("Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8") |>
      httr2::req_headers("Accept-Language" = "en-US,en;q=0.5") |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform()

    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

    status_code <- httr2::resp_status(resp)
    content <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) ""
    )

    blocking <- detect_blocking(status_code, content, response_time)

    tibble::tibble(
      url = url,
      user_agent = user_agent,
      status_code = status_code,
      response_time = response_time,
      is_blocked = blocking$is_blocked,
      reason = blocking$reason,
      score = blocking$score
    )
  }, error = function(e) {
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

    tibble::tibble(
      url = url,
      user_agent = user_agent,
      status_code = NA_integer_,
      response_time = response_time,
      is_blocked = TRUE,
      reason = paste0("Error: ", e$message),
      score = 0
    )
  })

  result
}

#' Test a user agent against multiple sites
#'
#' Tests a single user agent string against a list of websites and returns
#' aggregated results including a blocking score.
#'
#' @param user_agent User agent string to test
#' @param sites Character vector of URLs to test against (default: default_test_sites())
#' @param delay Delay between requests in seconds (default 0.5)
#' @return A tibble with per-site results and an overall blocking score
#' @export
#' @examples
#' \dontrun{
#' results <- test_ua_all_sites(
#'   "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0.0.0",
#'   sites = c("https://httpbin.org/user-agent", "https://www.google.com/")
#' )
#' }
test_ua_all_sites <- function(user_agent, sites = default_test_sites(), delay = 0.5) {
  results <- purrr::map_dfr(sites, function(site) {
    result <- test_user_agent(site, user_agent)
    Sys.sleep(delay)
    result
  })

  results
}

#' Calculate blocking score from test results
#'
#' Computes an overall blocking score (0-100) from individual site test results.
#' Higher scores indicate better success rates (less blocking).
#'
#' @param test_results A tibble from test_ua_all_sites() with a 'score' column
#' @return Numeric blocking score from 0 (always blocked) to 100 (never blocked)
#' @export
#' @examples
#' \dontrun{
#' results <- test_ua_all_sites("Mozilla/5.0...")
#' score <- calculate_blocking_score(results)
#' }
calculate_blocking_score <- function(test_results) {
  mean(test_results$score, na.rm = TRUE) * 100
}

#' Run comprehensive blocking test on user agents
#'
#' Tests a sample of user agents against multiple websites to determine
#' which user agents are most effective for web scraping.
#'
#' @param user_agents A tibble with user agent data (must have 'useragent_string' column)
#' @param sites Character vector of URLs to test against
#' @param delay Delay between requests in seconds (default 0.5)
#' @param progress Show progress bar (default TRUE)
#' @return A tibble with user agent info and blocking scores
#' @export
#' @examples
#' \dontrun{
#' # Load user agent data
#' ua_data <- readRDS("data/chrome.rds")
#' sample_ua <- ua_data[1:10, ]
#'
#' # Run blocking test
#' results <- run_blocking_test(sample_ua, sites = default_test_sites()[1:3])
#' }
run_blocking_test <- function(user_agents,
                              sites = default_test_sites(),
                              delay = 0.5,
                              progress = TRUE) {

  n_ua <- nrow(user_agents)
  n_sites <- length(sites)

  cli::cli_alert_info("Testing {n_ua} user agents against {n_sites} sites...")
  cli::cli_alert_info("Estimated time: {round(n_ua * n_sites * (delay + 1) / 60, 1)} minutes")

  results <- list()

  if (progress) {
    cli::cli_progress_bar("Testing user agents", total = n_ua)
  }

  for (i in seq_len(n_ua)) {
    ua <- user_agents$useragent_string[i]

    site_results <- test_ua_all_sites(ua, sites = sites, delay = delay)
    blocking_score <- calculate_blocking_score(site_results)

    # Aggregate per-site results
    per_site <- site_results |>
      dplyr::mutate(site_name = basename(url)) |>
      dplyr::select(site_name, score)

    results[[i]] <- tibble::tibble(
      browser = user_agents$browser[i],
      version = user_agents$version[i],
      useragent_string = ua,
      first_visit = user_agents$first_visit[i],
      last_visit = user_agents$last_visit[i],
      blocking_score = blocking_score,
      n_sites_tested = n_sites,
      n_blocked = sum(site_results$is_blocked),
      avg_response_time = mean(site_results$response_time, na.rm = TRUE)
    )

    if (progress) {
      cli::cli_progress_update()
    }
  }

  if (progress) {
    cli::cli_progress_done()
  }

  dplyr::bind_rows(results)
}
