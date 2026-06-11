# Add Stations

Add Stations

## Usage

``` r
ts_add_stations(stations, conn = getOption("tsdbr.conn", NULL))
```

## Arguments

- stations:

  A data frame of stations with columns Station, Parameter, Site and
  Period. The optional columns are Depth, LowerLimit, UpperLimit,
  StationName, StationID and Comments.

- conn:

  An object of class SQLiteConnection.

## Value

The imported station data.
