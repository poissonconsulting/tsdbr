# Translate Stations

Translate stations from or to the codings in Station, StationName and
StationID. Its useful if the data are provided with different station
codings to the main ones used in the database.

## Usage

``` r
ts_translate_stations(
  data,
  from = "StationID",
  to = "Station",
  na_rm = TRUE,
  conn = getOption("tsdbr.conn", NULL)
)
```

## Arguments

- data:

  A data frame with the column Station of the station coding.

- from:

  A string indicating the coding to translate from. Possible values are
  'Station', 'StationName' and 'StationID'.

- to:

  A string indicating the coding to translate to.

- na_rm:

  A flag indicating whether to remove missing stations. Possible values
  are 'Station', 'StationName' and 'StationID'.

- conn:

  An object of class SQLiteConnection.

## Value

The translated data
