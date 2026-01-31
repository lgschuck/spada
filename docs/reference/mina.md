# Minimum value taking NA's into account

Minimum value taking NA's into account

## Usage

``` r
mina(x, na_rm = TRUE)
```

## Arguments

- x:

  Object with values

- na_rm:

  If TRUE or T removes NA's. Default value is T.

## Examples

``` r
mina(c(-1, 2, 3, NA))
#> [1] -1
mina(c(1, 2, 3, NA), na_rm = FALSE)
#> [1] NA
mina(c(1, 2, 3, NA, NaN), na_rm = FALSE)
#> [1] NA
```
