# UTC Offset to Time Zone

UTC Offset to Time Zone

## Usage

``` r
ts_utc_offset_to_tz(utc_offset)
```

## Arguments

- utc_offset:

  An integer of the UTC offset.

## Value

The time zone as a string.

## Examples

``` r
ts_utc_offset_to_tz(-8L)
#> [1] "Etc/GMT+8"
```
