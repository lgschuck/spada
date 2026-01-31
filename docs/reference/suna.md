# Sum taking NA's into account

Sum taking NA's into account

## Usage

``` r
suna(x, na_rm = TRUE)
```

## Arguments

- x:

  Object to be summed

- na_rm:

  If TRUE or T removes NA's. Default value is T.

## Examples

``` r
suna(c(1, 2, 3, NA))
#> [1] 6
suna(c(1, 2, 3, NA), na_rm = FALSE)
#> [1] NA
```
