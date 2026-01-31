# First n value taking NA's into account

If all values are NA returns NA

## Usage

``` r
fina(x, n = 1L, na_rm = TRUE)
```

## Arguments

- x:

  Object with values

- n:

  Position to return, default is 1 (first). If zero or less it will be
  corrected to 1

- na_rm:

  If TRUE or T will remove NA's. Default value is T.

## Examples

``` r
fina(c(5, 1, 2, 3, NA))
#> [1] 5
fina(c(5, 1, 2, 3, NA), n = 2)
#> [1] 1
fina(c(NA, 1, 2, 3, NA), na_rm = TRUE)
#> [1] 1
fina(c(NA, 1, 2, 3, NA), na_rm = FALSE)
#> [1] NA
fina(c(NA, NA, NA), na_rm = TRUE)
#> [1] NA
fina(c(NA, NA, NA), na_rm = FALSE)
#> [1] NA
```
