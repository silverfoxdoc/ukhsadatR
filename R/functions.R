#' Get Paginated Data
#'
#' Generates API query URL and retrieves paginated data.
#'
#' @param theme           the largest overall topical subgroup of data. For
#'                        example \code{infectious_disease}.
#'
#' @param sub_theme       a topical subgroup associated with the parent theme.
#'                        For example \code{respiratory}.
#'
#' @param topic           categorical subgroup associated with the selected
#'                        theme and sub_theme. For example, \code{COVID-19}.
#'
#' @param geography_type  the overarching area type for the intended geography.
#'                        For example \code{Nation}.
#'
#' @param geography       the selected area under the \code{geography_type}.
#'                        For example \code{England}.
#'
#' @param metric          the type of data being selected.
#'                        For example \code{COVID-19_testing_PCRcountByDay}.
#'
#' @param page_number     define which page of data to retrieve
#'
#' @param page_size       define number of results returned per page.
#'                        Maximum supported size is 365.
#' 
#' @return                list containing the query structure and results
#'
#' @details               If any input argument is left NULL or invalid, a list of possible values for that parameter is
#'                        returned.
#'
#' @importFrom            httr2 request req_url_query req_timeout req_perform resp_body_string resp_status
#' @importFrom            jsonlite fromJSON
#' @importFrom            utils URLencode
#' 
#' @export
#'
#' @examples
#' data <- get_paginated_data(
#'           theme = "infectious_disease",
#'           sub_theme = "respiratory",
#'           topic = "COVID-19",
#'           geography_type = "Nation",
#'           geography = "England",
#'           metric = "COVID-19_cases_casesByDay",
#'           page_number = 2
#'           )

get_paginated_data <- function(
  theme = NULL,
  sub_theme = NULL,
  topic = NULL,
  geography_type = NULL,
  geography = NULL,
  metric = NULL,
  page_number = 1,
  page_size = 365
) {
  # Ensure empty strings are converted to NULL
  clean_argument <- function(x) if (is.null(x) || x == "") NULL else x

  theme <- clean_argument(theme)
  sub_theme <- clean_argument(sub_theme)
  topic <- clean_argument(topic)
  geography_type <- clean_argument(geography_type)
  geography <- clean_argument(geography)
  metric <- clean_argument(metric)

  # Validate page_size
  if (page_size > 365) {
    stop("Maximum allowed page_size is 365")
  }

  # Base API URL
  base <- "https://api.ukhsa-dashboard.data.gov.uk/themes"

  # Helper to get options and validate
  get_and_validate <- function(url, value, label) {
    json_result <- httr2::request(url) |>
      httr2::req_timeout(10) |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON()

    options <- json_result[["name"]]

    if (is.null(options) || length(options) == 0) {
      stop(sprintf(
        "Unable to retrieve valid %s options from API. Please check the upstream API response or try again later.",
        label
      ))
    }

    if (is.null(value)) {
      stop(sprintf(
        "No %s entered. Available options:\n\n%s",
        label,
        paste(options, collapse = "\n")
      ))
    }

    if (!value %in% options) {
      stop(sprintf(
        "Invalid %s: '%s'. Available options:\n\n%s",
        label,
        value,
        paste(options, collapse = "\n")
      ))
    }

    return(value)
  }

  # Sequential validation and path construction
  if (is.null(theme)) {
    return(get_and_validate(base, NULL, "Theme"))
  }
  theme <- get_and_validate(base, theme, "Theme")

  base <- paste(base, utils::URLencode(theme), "sub_themes", sep = "/")
  if (is.null(sub_theme)) {
    return(get_and_validate(base, NULL, "Sub-theme"))
  }
  sub_theme <- get_and_validate(base, sub_theme, "Sub-theme")

  base <- paste(base, utils::URLencode(sub_theme), "topics", sep = "/")
  if (is.null(topic)) {
    return(get_and_validate(base, NULL, "Topic"))
  }
  topic <- get_and_validate(base, topic, "Topic")

  base <- paste(base, utils::URLencode(topic), "geography_types", sep = "/")
  if (is.null(geography_type)) {
    return(get_and_validate(base, NULL, "Geography Type"))
  }
  geography_type <- get_and_validate(base, geography_type, "Geography Type")

  base <- paste(
    base,
    utils::URLencode(geography_type),
    "geographies",
    sep = "/"
  )
  if (is.null(geography)) {
    return(get_and_validate(base, NULL, "Geography"))
  }
  geography <- get_and_validate(base, geography, "Geography")

  base <- paste(base, utils::URLencode(geography), "metrics", sep = "/")
  if (is.null(metric)) {
    return(get_and_validate(base, NULL, "Metric"))
  }
  metric <- get_and_validate(base, metric, "Metric")

  url <- paste(base, utils::URLencode(metric), sep = "/")

  # Paginated request
  response <- httr2::request(url) |>
    httr2::req_url_query(page_size = page_size, page = page_number) |>
    httr2::req_perform()

  if (response$status_code >= 400) {
    stop(sprintf(
      "API request failed [%s]: %s",
      response$status_code,
      httr2::resp_status(response)
    ))
  }

  result <- httr2::resp_body_string(response) |>
    jsonlite::fromJSON()

  return(result)
}




#' Get Data
#'
#' Iteratively runs \code{\link{get_paginated_data}} to download all results and
#' combine them into a `data.frame`.
#'
#' For further information on the UKHSA dashboard API please visit
#' the \href{https://ukhsa-dashboard.data.gov.uk/access-our-data}{API documentation}.
#'
#' @param theme           the largest overall topical subgroup of data. For
#'                        example \code{infectious_disease}.
#'
#' @param sub_theme       a topical subgroup associated with the parent theme.
#'                        For example \code{respiratory}.
#'
#' @param topic           categorical subgroup associated with the selected
#'                        theme and sub_theme. For example, \code{COVID-19}.
#'
#' @param geography_type  the overarching area type for the intended geography.
#'                        For example \code{Nation}.
#'
#' @param geography       the selected area under the \code{geography_type}.
#'                        For example \code{England}.
#'
#' @param metric          the type of data being selected.
#'                        For example \code{COVID-19_testing_PCRcountByDay}.
#'
#' @return                data.frame containing the query results
#'
#' @details               If any input argument is left NULL or invalid, a list of possible values for that parameter is
#'                        returned.
#'
#' @export
#'
#' @examples
#' data <- get_data(
#'           theme = "infectious_disease",
#'           sub_theme = "respiratory",
#'           topic = "COVID-19",
#'           geography_type = "Nation",
#'           geography = "England",
#'           metric = "COVID-19_cases_casesByDay"
#'           )

get_data <- function(
  theme = NULL,
  sub_theme = NULL,
  topic = NULL,
  geography_type = NULL,
  geography = NULL,
  metric = NULL
) {
  current_page <- 1
  results <- list()

  repeat {
    paginated <- get_paginated_data(
      theme = theme,
      sub_theme = sub_theme,
      topic = topic,
      geography_type = geography_type,
      geography = geography,
      metric = metric,
      page_number = current_page
    )

    if (is.null(paginated)) {
      break
    }

    if (!is.list(paginated)) {
      return(paginated)
    }

    if (length(paginated$results) == 0) {
      break
    }

    results[[length(results) + 1]] <- paginated$results

    if (is.null(paginated$`next`)) {
      break
    }

    current_page <- current_page + 1
  }

  if (length(results) == 0) {
    warning("No results were returned by the API.")
    return(data.frame())
  }

  do.call(rbind.data.frame, results)
}
