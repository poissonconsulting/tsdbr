# Add Site

Add Site

## Usage

``` r
ts_add_sites(sites, conn = getOption("tsdbr.conn", NULL))
```

## Arguments

- sites:

  A data frame of parameters with columns Site. The optional columns are
  Longitude, Latitude, Organization, SiteName and Comments.

- conn:

  An object of class SQLiteConnection.

## Value

A data frame of the imported parameters.
