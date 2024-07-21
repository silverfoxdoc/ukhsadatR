# UKHSA Data Dashboard API `R` Wrapper

This is a wrapper for the [UKHSA Data Dashboard](https://ukhsa-dashboard.data.gov.uk/) API, so that this data can be easily imported natively into `R`.

For further detailed documentation on the UKHSA data dashboard API is available at [](https://ukhsa-dashboard.data.gov.uk/access-our-data).

This package based upon the `ukcovid19` package previously published by Public Health England; the source code for this package is housed at [](https://github.com/publichealthengland/coronavirus-dashboard-api-r-sdk).


## Installation

At present this package is only available on github and can be installed as follows:

``` r
remotes::install_github("silverfoxdoc/coronavirus-dashboard-api-R-sdk")
```

## Pagination

This API wrapper will bypass the pagination process and always download the full dataset for the specified topic, however only a single metric can be downloaded at a time.

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
