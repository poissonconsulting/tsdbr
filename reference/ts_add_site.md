# Add Sites

Add Sites

## Usage

``` r
ts_add_site(
  site,
  longitude = NA_real_,
  latitude = NA_real_,
  organization = NA_character_,
  site_name = NA_character_,
  comments = NA_character_,
  conn = getOption("tsdbr.conn", NULL)
)
```

## Arguments

- site:

  A string of the site name.

- longitude:

  A numeric of the site longitude.

- latitude:

  A numeric of the site latitude.

- organization:

  A string of the organization name.

- site_name:

  A string of the site name.

- comments:

  A string of site comments.

- conn:

  An object of class SQLiteConnection.

## Value

A data frame of the imported parameters.
