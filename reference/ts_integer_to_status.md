# Status to Integer

Converts an integer vector to an ordered factor of status values.

## Usage

``` r
ts_integer_to_status(x)
```

## Arguments

- x:

  An integer vector.

## Value

An ordered factor of status values.

## Examples

``` r
ts_integer_to_status(1:3)
#> [1] reasonable   questionable erroneous   
#> Levels: reasonable < questionable < erroneous
```
