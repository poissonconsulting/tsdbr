# Add Data

Times are rounded down prior to import.

## Usage

``` r
ts_add_data(
  data,
  aggregate = NULL,
  na_rm = FALSE,
  resolution = "abort",
  conn = getOption("tsdbr.conn", NULL)
)
```

## Arguments

- data:

  A data frame of data with columns Station, DateTime, Recorded.
  Additional optional columns include Corrected, Status, Comments.

- aggregate:

  A function to aggregate multiple values within the same station
  period.

- na_rm:

  A flag indicating whether to remove missing values (if possible) when
  aggregating.

- resolution:

  A string of the action to take with regard to existing values. Options
  are 'abort', 'ignore' or 'replace'.

- conn:

  An object of class SQLiteConnection.

## Value

A data frame of the imported parameters.
