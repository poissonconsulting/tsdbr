# Status to Integer

Converts an ordered factor of status values to an integer vector.

## Usage

``` r
ts_status_to_integer(x)
```

## Arguments

- x:

  An ordered factor of status values.

## Value

An integer vector.

## Examples

``` r
ts_status_to_integer(ordered(
  "questionable",
  c("reasonable", "questionable", "erroneous")
))
#> [1] 2
```
