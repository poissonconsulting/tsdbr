# Add Parameters

Add Parameters

## Usage

``` r
ts_add_parameters(parameters, conn = getOption("tsdbr.conn", NULL))
```

## Arguments

- parameters:

  A data frame of parameters with columns Parameter and Units.

- conn:

  An object of class SQLiteConnection.

## Value

A data frame of the imported parameters.
