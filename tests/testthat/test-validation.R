library(testthat)
library(ukhsadatR)

# These tests require internet access to the UKHSA API
# and are skipped on CRAN to avoid test failures due to network issues.

test_that("get_paginated_data errors with no value and shows options for theme", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = NULL
    ),
    regexp = "No Theme entered.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with invalid theme and shows options", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "NOT_A_THEME"
    ),
    regexp = "Invalid Theme.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with no value and shows options for sub_theme", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = NULL
    ),
    regexp = "No Sub-theme entered.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with invalid sub_theme and shows options", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "NOT_A_SUB_THEME"
    ),
    regexp = "Invalid Sub-theme.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with no value and shows options for topic", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = NULL
    ),
    regexp = "No Topic entered.*Available options.*COVID-19",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with invalid topic and shows options", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = "NOT_A_TOPIC"
    ),
    regexp = "Invalid Topic.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with no value and shows options for geography_type", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = "COVID-19",
      geography_type = NULL
    ),
    regexp = "No Geography Type entered.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with invalid geography_type and shows options", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = "COVID-19",
      geography_type = "NOT_A_GEOGRAPHY_TYPE"
    ),
    regexp = "Invalid Geography Type.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with no value and shows options for geography", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = "COVID-19",
      geography_type = "Nation",
      geography = NULL
    ),
    regexp = "No Geography entered.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with invalid geography and shows options", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = "COVID-19",
      geography_type = "Nation",
      geography = "NOT_A_GEOGRAPHY"
    ),
    regexp = "Invalid Geography.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with no value and shows options for metric", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = "COVID-19",
      geography_type = "Nation",
      geography = "England",
      metric = NULL
    ),
    regexp = "No Metric entered.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors with invalid metric and shows options", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = "COVID-19",
      geography_type = "Nation",
      geography = "England",
      metric = "NOT_A_METRIC"
    ),
    regexp = "Invalid Metric.*Available options",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data errors on page_size > 365", {
  testthat::skip_on_cran()
  expect_error(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = "COVID-19",
      geography_type = "Nation",
      geography = "England",
      metric = "COVID-19_cases_casesByDay",
      page_size = 366
    ),
    regexp = "Maximum allowed page_size is 365",
    ignore.case = TRUE
  )
})

test_that("get_paginated_data accepts page_size = 365", {
  testthat::skip_on_cran()
  expect_silent(
    get_paginated_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = "COVID-19",
      geography_type = "Nation",
      geography = "England",
      metric = "COVID-19_cases_casesByDay",
      page_size = 365
    )
  )
})

test_that("get_data returns a data.frame with valid inputs", {
  testthat::skip_on_cran()
  df <- get_data(
    theme = "infectious_disease",
    sub_theme = "respiratory",
    topic = "COVID-19",
    geography_type = "Nation",
    geography = "England",
    metric = "COVID-19_cases_casesByDay"
  )
  expect_s3_class(df, "data.frame")
  expect_gte(nrow(df), 0)
})
