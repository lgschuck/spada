# Last n value taking NA's into account

If all values are NA returns NA

## Usage

``` r
lana(x, n = 1, na_rm = TRUE)
```

## Arguments

- x:

  Object with values

- n:

  Position to return, default is 1 (last). If zero or less it will be
  corrected to 1.

- na_rm:

  If TRUE or T removes the NA's. Default value is T.

## Examples

``` r
lana(c(5, 1, 2, 3, NA))
#> [1] 3
lana(c(NA, 1, 2, 3, NA), na_rm = FALSE)
#> [1] NA
lana(c(5, 1, 2, 3, NA), n = 3)
#> [1] 1
lana(c(5, 1, 2, 3, NA), n = 3)
#> [1] 1
lana(c(NA, NA, NA), na_rm = TRUE)
#> [1] NA
lana(c(NA, NA, NA), na_rm = FALSE)
#> [1] NA
```
