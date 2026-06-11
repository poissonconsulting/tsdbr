# Set Maintainer

Sets maintainer.

## Usage

``` r
ts_set_maintainer(
  maintainer = ts_sys_user(),
  conn = getOption("tsdbr.conn", NULL)
)
```

## Arguments

- maintainer:

  A string of the maintainer.

- conn:

  An object of class SQLiteConnection.

## Value

A string of the old maintainer.
