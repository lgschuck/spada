# Maximum value taking NA's into account

Maximum value taking NA's into account

## Usage

``` r
mana(x, na_rm = TRUE)
```

## Arguments

- x:

  Object with values

- na_rm:

  If TRUE or T removes NA's. Default value is T.

## Examples

``` r
mana(c(-1, 2, 3, NA))
#> [1] 3
mana(c(1, 2, 3, NA), na_rm = FALSE)
#> [1] NA
mana(c(1, 2, 3, NA, NaN), na_rm = FALSE)
#> [1] NA
```
