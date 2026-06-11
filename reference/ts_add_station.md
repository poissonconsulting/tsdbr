# Add Station

Add Station

## Usage

``` r
ts_add_station(
  station,
  parameter,
  site,
  period,
  lower_limit = NA_real_,
  upper_limit = NA_real_,
  depth = NA_real_,
  station_name = NA_character_,
  station_id = NA_character_,
  comments = NA_character_,
  conn = getOption("tsdbr.conn", NULL)
)
```

## Arguments

- station:

  A string of the station.

- parameter:

  A string of the parameter.

- site:

  A string of the site.

- period:

  A string of the period. The possible values are 'year', 'month',
  'day', 'hour', 'minute' and 'second'.

- lower_limit:

  A numeric of the lower limit.

- upper_limit:

  A numeric of the upper limit.

- depth:

  A numeric of the depth.

- station_name:

  A string of the station name.

- station_id:

  A string of the station id.

- comments:

  A string of the station comments.

- conn:

  An object of class SQLiteConnection.

## Value

A data frame of the imported station.
