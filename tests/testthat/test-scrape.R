test_that("get_browser_links returns character vector for Chrome", {
  skip_on_cran()

  links <- get_browser_links("Chrome")

  expect_type(links, "character")
  expect_true(length(links) > 0)
  expect_true(all(grepl("_id_\\d+\\.php$", links)))
})

test_that("get_useragent_details returns correct structure", {
  skip_on_cran()

  # Use a known Chrome detail URL
  links <- get_browser_links("Chrome")
  skip_if(length(links) == 0, "No Chrome links available")

  details <- get_useragent_details(links[1])

  expect_s3_class(details, "tbl_df")
  expect_named(details, c("useragent_string", "first_visit", "last_visit", "detail_url"))
  expect_equal(nrow(details), 1)
})

test_that("get_useragent_details extracts dates correctly", {
  skip_on_cran()

  links <- get_browser_links("Chrome")
  skip_if(length(links) == 0, "No Chrome links available")

  details <- get_useragent_details(links[1])

  # Check date format (YYYY.MM.DD HH:MM or NA)
  if (!is.na(details$first_visit)) {
    expect_match(details$first_visit, "^\\d{4}\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2}$")
  }
  if (!is.na(details$last_visit)) {
    expect_match(details$last_visit, "^\\d{4}\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2}$")
  }
})

test_that("scrape_browser works with limit parameter", {
  skip_on_cran()

  # Test with just 3 entries to keep it fast
  result <- scrape_browser("Chrome", parallel = FALSE, limit = 3)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("browser", "version", "useragent_string", "first_visit", "last_visit", "detail_url"))
  expect_equal(nrow(result), 3)
  expect_true(all(result$browser == "Chrome"))
})

test_that("scrape_browser handles non-existent browser gracefully", {
  skip_on_cran()

  result <- scrape_browser("NonExistentBrowser123", parallel = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
