# Get Data

Get Data

## Usage

``` r
ts_get_data(
  stations = NULL,
  start_date = NULL,
  end_date = NULL,
  period = "hour",
  na_rm = FALSE,
  status = "questionable",
  fill = TRUE,
  conn = getOption("tsdbr.conn", NULL)
)
```

## Arguments

- stations:

  A character vector of the stations.

- start_date:

  The start date.

- end_date:

  The end date.

- period:

  A string of the period to aggregate and average by. The possible
  values are 'year', 'month', 'day', 'hour', 'minute' and 'second'.

- na_rm:

  A flag indicating whether to remove missing values (if possible) when
  aggregating.

- status:

  A string of the worse type of data to get. The possible values are
  'reasonable', 'questionable' or 'erroneous'.

- fill:

  A flag indicating whether to fill in gaps (with missing values).

- conn:

  An object of class SQLiteConnection.

## Value

A data frame of the requested data.
