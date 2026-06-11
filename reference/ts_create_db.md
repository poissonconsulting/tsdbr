# Create Time-Series Database

Creates an empty SQLite database to store time series data. The UTC
offset for Alaska is -8.

## Usage

``` r
ts_create_db(
  file,
  utc_offset = 0L,
  periods = c("year", "month", "day", "hour", "minute", "second")
)
```

## Arguments

- file:

  A string of the name of the database file.

- utc_offset:

  A integer of the utc offset which must lie between -12 and 14.

- periods:

  A character vector of the permitted periods. Possible values are
  'year', 'month', 'day', 'hour', 'minute', 'second'

## Value

A connection to the database.
