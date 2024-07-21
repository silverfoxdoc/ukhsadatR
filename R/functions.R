#' Get Paginated Data
#' 
#' Generates API query URL, retrieves paginated data and parses results into JSON format. If an API parameter 
#' is not provided then a list of possible parameters is returned.
#' 
#' @param theme           the largest overall topical subgroup of data for example \code{infectious_disease}.
#' 
#' @param sub_theme       a topical subgroup associated with the parent theme. for example \code{respiratory}.
#'                   
#' @param topic           categorical subgroup associated with the selected theme and sub_theme. For example a topic 
#' of \code{COVID-19} would only be available for a theme of \code{infectious_disease} and a sub_theme of 
#' \code{respiratory}.
#' 
#' @param geography_type  the overarching area type for the intended geography for example \code{Nation}.
#' 
#' @param geography       the selected area under the \code{geography_type}. For example \code{England}.
#' 
#' @param metric          the type of data being selected. For example \code{COVID-19_testing_PCRcountByDay}.
#' 
#' @param page_number     define which page of data to retrieve
#' 
#' @param page_size       define number of results returned per page. Maximum supported size is 365.
#' 
#' @return                Data for the given query or a list of parameter options.
#' 
#' @importFrom            httr2 request req_url_query req_timeout req_perform resp_body_string resp_status
#' @importFrom            jsonlite fromJSON

get_paginated_data <- function(theme = NULL, 
                               sub_theme = NULL, 
                               topic = NULL, 
                               geography_type = NULL, 
                               geography = NULL, 
                               metric = NULL,
                               page_number = 1,
                               page_size = 365) {
  
  # change blank strings to NULL
  if(any(is.null(theme) | theme == "")) theme <- NULL
  if(any(is.null(sub_theme) | sub_theme == "")) sub_theme <- NULL
  if(any(is.null(topic) | topic == "")) topic <- NULL
  if(any(is.null(geography_type) | geography_type == "")) geography_type <- NULL
  if(any(is.null(geography) | geography == "")) geography <- NULL
  if(any(is.null(metric) | metric == "")) metric <- NULL
  
  # create API endpoint URL 
  if (is.null(theme)) {
    url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes")
  } else {
    if (is.null(sub_theme)) {
      url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", gsub(" ", "%20", theme), "sub_themes", sep = "/")
    } else {
      if (is.null(topic)) {
        url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", gsub(" ", "%20", theme), "sub_themes", gsub(" ", "%20", sub_theme), "topics", sep = "/")
      } else {
        if (is.null(geography_type)) {
          url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", gsub(" ", "%20", theme), "sub_themes", gsub(" ", "%20", sub_theme), "topics", gsub(" ", "%20", topic), "geography_types", sep = "/")
        } else {
          if (is.null(geography)) {
            url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", gsub(" ", "%20", theme), "sub_themes", gsub(" ", "%20", sub_theme), "topics", gsub(" ", "%20", topic), "geography_types", gsub(" ", "%20", geography_type), "geographies", sep = "/")
          } else {
            if (is.null(metric)) {
              url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", gsub(" ", "%20", theme), "sub_themes", gsub(" ", "%20", sub_theme), "topics", gsub(" ", "%20", topic), "geography_types", gsub(" ", "%20", geography_type), "geographies", gsub(" ", "%20", geography), "metrics", sep = "/")
            } else {
              url <- paste("https://api.ukhsa-dashboard.data.gov.uk/themes", gsub(" ", "%20", theme), "sub_themes", gsub(" ", "%20", sub_theme), "topics", gsub(" ", "%20", topic), "geography_types", gsub(" ", "%20", geography_type), "geographies", gsub(" ", "%20", geography), "metrics", gsub(" ", "%20", metric), sep = "/")
            }
          }
        }
      }
    }
  }
  
  # get response for url for use in error control
  response <- httr2::request(url) |>
    httr2::req_timeout(10) |>
    httr2::req_perform()
  
  # get parameters when query not fully completed
  parameters <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |> 
    _[["name"]]
  
  # Handle errors; return parameters if not completed fully; return result if query fully entered
  if (response$status_code >= 400) {
    stop(httr2::resp_status(response))
  } else {
    if (response$status_code == 204) {
      response <- NULL
    } else {
      if(is.null(theme)) {
        writeLines("\nTheme:\n")
        return(parameters)
      } else {
        if (is.null(sub_theme)) {
          writeLines("\nSub Theme:\n")
          return(parameters)
        } else {
          if (is.null(topic)) {
            writeLines("\nTopic:\n")
            return(parameters)
          } else {
            if (is.null(geography_type)) {
              writeLines("\nGeography Type:\n")
              return(parameters)
            } else {
              if (is.null(geography)) {
                writeLines("\nGeography:\n")
                return(parameters)
              } else {
                if (is.null(metric)) {
                  writeLines("\nMetric:\n")
                  return(parameters)
                } else {
                  # add page size/number query & return paginated results
                  result <- httr2::request(url) |> 
                    httr2::req_url_query(page_size = page_size, page = page_number) |> 
                    httr2::req_perform() |> 
                    httr2::resp_body_string() |> 
                    jsonlite::fromJSON()
                  
                  return(result)
                }
              }
            }
          }
        }
      }
    }
  }
}


#' Get Data
#' 
#' Iteratively runs query to download all results and combine them into a `data.frame`.
#' 
#' For further information on the UKHSA dashboard API please visit 
#' the \href{https://ukhsa-dashboard.data.gov.uk/access-our-data}{API documentation}.
#' 
#' @param theme           the largest overall topical subgroup of data, for example \code{infectious_disease}.
#' 
#' @param sub_theme       a topical subgroup associated with the parent theme, for example \code{respiratory}.
#'                   
#' @param topic           categorical subgroup associated with the selected theme and sub_theme. For example a topic 
#' of \code{COVID-19} would only be available for a theme of \code{infectious_disease} and a sub_theme of 
#' \code{respiratory}.
#' 
#' @param geography_type  the overarching area type for the intended geography, for example \code{Nation}.
#' 
#' @param geography       the selected area under the \code{geography_type}, for example \code{England}.
#' 
#' @param metric          the type of data being selected, for example \code{COVID-19_testing_PCRcountByDay}.
#' 
#' @return                List of parameter options.
#'                   
#' @return                data.frame for the given query.
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

    paginated_results <- get_paginated_data(
      theme = theme,
      sub_theme = sub_theme,
      topic = topic,
      geography_type = geography_type,
      geography = geography,
      metric = metric,
      page_number = current_page
    )
    
    if (is.null(paginated_results)) {
      break
    }
    
    if (!is.list(paginated_results)) {
      return(paginated_results)
    }
    
    results <- rbind(results, paginated_results$results)
    
    if (is.null(paginated_results$`next`)) {
      break
    }
    
    current_page <- current_page + 1

  }
  
  return(results)
  
}
