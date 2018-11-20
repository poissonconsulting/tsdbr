
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/poissonconsulting/tsdbr.svg?branch=master)](https://travis-ci.org/poissonconsulting/tsdbr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/tsdbr?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/tsdbr)
[![Coverage
status](https://codecov.io/gh/poissonconsulting/tsdbr/branch/master/graph/badge.svg)](https://codecov.io/github/poissonconsulting/tsdbr?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

# tsdbr

`tsdbr` is designed to create, populate, maintain and query SQLite
databases of environmental time series data.

## Utilisation

``` r
library(tsdbr)
conn <- ts_create_db(":memory:")
options(tsdbr.conn = conn)
ts_add_parameter("discharge", "cms")
ts_add_site("Niagara Falls")
ts_add_station("S1", "discharge", "Niagara Falls", "hour")
data <- data.frame(Station = "S1", DateTime = ISOdate(2000, 9, 1, 0:23),
                   Recorded = 0:23 - 2,
                   stringsAsFactors = FALSE)
ts_add_data(data)
data <- ts_get_data(start_date = as.Date("2000-09-01"),
                    end_date = as.Date("2000-09-02"))
print(head(data))
#> # A tibble: 6 x 11
#>   Station DateTime            Recorded Corrected Status Site  Depth
#>   <chr>   <dttm>                 <dbl>     <dbl> <ord>  <chr> <dbl>
#> 1 S1      2000-09-01 00:00:00       -2        -2 reaso… Niag…    NA
#> 2 S1      2000-09-01 01:00:00       -1        -1 reaso… Niag…    NA
#> 3 S1      2000-09-01 02:00:00        0         0 reaso… Niag…    NA
#> 4 S1      2000-09-01 03:00:00        1         1 reaso… Niag…    NA
#> 5 S1      2000-09-01 04:00:00        2         2 reaso… Niag…    NA
#> 6 S1      2000-09-01 05:00:00        3         3 reaso… Niag…    NA
#> # ... with 4 more variables: Parameter <chr>, Units <chr>,
#> #   StationName <chr>, Comments <chr>
ts_plot_data(data)
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

``` r
ts_disconnect_db()
```

## Installation

To install the latest version from the Poisson drat
[repository](https://github.com/poissonconsulting/drat)

``` r
install.packages("drat")
drat::addRepo("poissonconsulting")
install.packages("tsdbr")
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/tsdbr/issues).

[Pull requests](https://github.com/poissonconsulting/tsdbr/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms
