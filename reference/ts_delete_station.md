# Delete Station

Deletes all records associated with a station from the database.

## Usage

``` r
ts_delete_station(station, conn = getOption("tsdbr.conn", NULL))
```

## Arguments

- station:

  A string of the station name.

- conn:

  An object of class SQLiteConnection.
