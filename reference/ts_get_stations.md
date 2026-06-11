# Get Stations Table

Gets stations table as a data frame.

## Usage

``` r
ts_get_stations(
  parameters = NULL,
  periods = c("year", "month", "day", "hour", "minute", "second"),
  sites = NULL,
  conn = getOption("tsdbr.conn", NULL)
)
```

## Arguments

- parameters:

  A character of the parameters to filter by.

- periods:

  A character vector of the periods to filter by.

- sites:

  A character of the sites to filter by.

- conn:

  An object of class SQLiteConnection.

## Value

A data frame of the requested data.
