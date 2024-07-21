# UKHSA Data Dashboard API `R` Wrapper

This is a wrapper for the [UKHSA Data Dashboard](https://ukhsa-dashboard.data.gov.uk/) API, so that this data can be easily imported natively into `R`.

Further detailed documentation on the UKHSA data dashboard API is available [here](https://ukhsa-dashboard.data.gov.uk/access-our-data).

This package is based upon the `ukcovid19` package previously published by Public Health England; the source code for this package is housed [here](https://github.com/publichealthengland/coronavirus-dashboard-api-r-sdk).


## Installation

At present this package is only available on github and can be installed as follows:

``` r
remotes::install_github("silverfoxdoc/UKHSAdashR")
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
