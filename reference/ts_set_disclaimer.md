# Set Disclaimer

Sets disclaimer.

## Usage

``` r
ts_set_disclaimer(
  disclaimer = "THE DATA ARE COPYRIGHTED",
  conn = getOption("tsdbr.conn", NULL)
)
```

## Arguments

- disclaimer:

  A string of the disclaimer.

- conn:

  An object of class SQLiteConnection.

## Value

A string of the old disclaimer.
