# Add Parameter

Add Parameter

## Usage

``` r
ts_add_parameter(parameter, units, conn = getOption("tsdbr.conn", NULL))
```

## Arguments

- parameter:

  A string of the parameter name.

- units:

  A string of the units

- conn:

  An object of class SQLiteConnection.

## Value

A data frame of the imported parameters.
