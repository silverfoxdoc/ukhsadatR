library(tibble)
library(tidyr)
library(purrr)
library(httr2)


#' Get Paginated Data
#' 
#' Generates API query URL, retrieves paginated data and parses results into JSON format. If an API parameter 
#' is not provided then a list of possible parameters is returned.
#' 
#' @param theme      the largest overall topical subgroup of data for example \code{infectious_disease}.
#' 
#' @param sub_theme  a topical subgroup associated with the parent theme. for example \code{respiratory}.
#'                   
#' @param topic      categorical subgroup associated with the selected theme and sub_theme. For example a topic 
#' of \code{COVID-19} would only be available for a theme of \code{infectious_disease} and a sub_theme of 
#' \code{respiratory}.
#' 
#' @param geography_type the overarching area type for the intended geography for example \code{Nation}.
#' 
#' @param geography   the selected area under the \code{geography_type}. For example \code{England}.
#' 
#' @param metric      the type of data being selected. For example \code{COVID-19_testing_PCRcountByDay}.
#' 
#' @param page_number define which page of data to retrieve
#' 
#' @param page_size   define number of results returned per page. Maximum supported size is 365.
#' 
#' @return            Data for the given query or a list of parameter options.

get_paginated_data <- function(theme = NULL, 
                               sub_theme = NULL, 
                               topic = NULL, 
                               geography_type = NULL, 
                               geography = NULL, 
                               metric = NULL,
                               page_number = 1,
                               page_size = 365) {
  
  # create API request URL 
  if (is.null(theme)) {
    url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes")
  } else {
    if (is.null(sub_theme)) {
      url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", theme, "sub_themes", sep = "/")
    } else {
      if (is.null(topic)) {
        url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", theme, "sub_themes", sub_theme, "topics", sep = "/")
      } else {
        if (is.null(geography_type)) {
          url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", theme, "sub_themes", sub_theme, "topics", topic, "geography_types", sep = "/")
        } else {
          if (is.null(geography)) {
            url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", theme, "sub_themes", sub_theme, "topics", topic, "geography_types", geography_type, "geographies", sep = "/")
          } else {
            if (is.null(metric)) {
              url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", theme, "sub_themes", sub_theme, "topics", topic, "geography_types", geography_type, "geographies", geography, "metrics", sep = "/")
            } else {
              url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", theme, "sub_themes", sub_theme, "topics", topic, "geography_types", geography_type, "geographies", geography, "metrics", metric, sep = "/")
            }
          }
        }
      }
    }
  }
  
  url <- stringr::str_replace_all(url, " ", "%20")
  
  # get response for url for use in error control
  response <- httr2::request(url) |>
    httr2::req_timeout(10) |> 
    httr2::req_perform()
  
  # Handle errors
  if(is.null(theme)|is.null(sub_theme)|is.null(topic)|is.null(geography_type)|is.null(geography)|is.null(metric)){
    # if parameters not filled out completely return possible parameters for that level
    result <- httr2::request(url) |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      stringr::str_extract_all('(?<=\\"name\\":\\")[^\\"]*') |>
      unlist()
  } else {
    if (response$status_code >= 400) {
      stop(httr2::resp_status(response))
    } else {
      if (response$status_code == 204) {
        response <- NULL
      } else {
        # add page size/number query & return data in JSON format
        result <- httr2::request(url) |> 
          httr2::req_url_query(page_size = page_size, page = page_number) |> 
          httr2::req_perform() |> 
          httr2::resp_body_json()
      }
    }
  }
  
  return(result)
  
}


#' Get Data
#' 
#' Interatively runs query to download all pages, combine the results and return a tibble.
#' 
#' For further information on the UKHSA dashboard API please visit 
#' the \href{https://ukhsa-dashboard.data.gov.uk/access-our-data}{API documentation}.
#' 
#' @param theme      the largest overall topical subgroup of data, for example \code{infectious_disease}.
#' 
#' @param sub_theme  a topical subgroup associated with the parent theme, for example \code{respiratory}.
#'                   
#' @param topic      categorical subgroup associated with the selected theme and sub_theme. For example a topic 
#' of \code{COVID-19} would only be available for a theme of \code{infectious_disease} and a sub_theme of 
#' \code{respiratory}.
#' 
#' @param geography_type the overarching area type for the intended geography, for example \code{Nation}.
#' 
#' @param geography  the selected area under the \code{geography_type}, for example \code{England}.
#' 
#' @param metric     the type of data being selected, for example \code{COVID-19_testing_PCRcountByDay}.
#' 
#' @return           List of parameter options.
#'                   
#' @return           Tibble of data for the given \code{parameters}.
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

get_data <- function(theme = NULL, 
                     sub_theme = NULL, 
                     topic = NULL, 
                     geography_type = NULL, 
                     geography = NULL, 
                     metric = NULL) {
  
  results <- list()
  current_page <- 1
  
  repeat {
    # print(current_page)  # for testing purposes
    paginated_results <- get_paginated_data(theme = theme, 
                                            sub_theme = sub_theme, 
                                            topic = topic, 
                                            geography_type = geography_type, 
                                            geography = geography, 
                                            metric = metric,
                                            page_number = current_page)
    
    if (is.null(paginated_results))
      break
    
    if (!("results" %in% names(paginated_results))) {
      return(paginated_results)
    }

    results <- c(results, paginated_results[["results"]])
    
    # print(paginated_results[["next"]]) # for testing purposes
    
    if (is.null(paginated_results[["next"]]))
      break
    
    current_page <- current_page + 1
    
  }
  
  results <- results |>
    purrr::transpose() |>
    tibble::as_tibble() |>
    tidyr::unnest(cols = tidyr::everything())
  
  return(results)
  
}
