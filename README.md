
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/poissonconsulting/tsdbr.svg?branch=master)](https://travis-ci.org/poissonconsulting/tsdbr)
[![Coverage
status](https://codecov.io/gh/poissonconsulting/tsdbr/branch/master/graph/badge.svg)](https://codecov.io/github/poissonconsulting/tsdbr?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

# tsdbr

tsdbr provide functions to create, populate, maintain and query an
SQLite database of environmental time series data.

tsdbr is designed to be simple to use with minimal dependencies and
maximal database integrity independence.

## Utilisation

``` r
library(tsdbr)
conn <- ts_create_db(":memory:")
options(tsdbr.conn = conn)
ts_add_parameter("discharge", "cms")
ts_add_site("Niagara Falls")
ts_add_station("S1", "discharge", "hour", "Niagara Falls")
ts_disconnect_db()
```

## Installation

To install the latest version from the Poisson drat
[repository](https://github.com/poissonconsulting/drat)

``` r
# install.packages("drat")
drat::addRepo("poissonconsulting")
install.packages("tsdbr")
```

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/tsdbr)

``` r
# install.packages("devtools")
devtools::install_github("poissonconsulting/tsdbr")
```

## Citation

``` 

To cite package 'tsdbr' in publications use:

  Joe Thorley (2018). tsdbr: Environmental Time Series Databasing.
  R package version 0.0.0.9008.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {tsdbr: Environmental Time Series Databasing},
    author = {Joe Thorley},
    year = {2018},
    note = {R package version 0.0.0.9008},
  }
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/tsdbr/issues).

[Pull requests](https://github.com/poissonconsulting/tsdbr/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
