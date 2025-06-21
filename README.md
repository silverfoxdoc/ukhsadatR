# ukhsadatR

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ukhsadatR)](https://CRAN.R-project.org/package=ukhsadatR)
[![R-CMD-check](https://github.com/silverfoxdoc/ukhsadatR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/silverfoxdoc/ukhsadatR/actions)
<!-- badges: end -->

This is a wrapper for the [UKHSA Data Dashboard](https://ukhsa-dashboard.data.gov.uk/) API, so that data can be easily imported into `R`.

Further detailed documentation on the UKHSA data dashboard API is available [here](https://ukhsa-dashboard.data.gov.uk/access-our-data).

This package is based upon the [`ukcovid19`](https://github.com/UKHSA-Internal/coronavirus-dashboard-api-R-sdk) package previously published by Public Health England but has been substantially rewritten and extended.

## Installation

To install the latest release version from CRAN:
``` r
install.packages("ukhsadatR")
```

To install the latest development version:
``` r
remotes::install_github("silverfoxdoc/ukhsadatR")
```

## Pagination

This API wrapper will bypass the pagination process and always download the full dataset for the specified metric, however, only a single metric can be downloaded at a time.

## Example

To obtain the daily reported cases of COVID-19, use the following:

``` r
get_data(
  theme = "infectious_disease",
  sub_theme = "respiratory",
  topic = "COVID-19",
  geography_type = "Nation",
  geography = "England",
  metric = "COVID-19_cases_casesByDay"
)
```
