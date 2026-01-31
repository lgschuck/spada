# Functions to calculate percentiles

Functions to calculate percentiles

## Usage

``` r
pn(y, p, na_rm = TRUE, ...)

p10(y, na_rm = TRUE, ...)

p25(y, na_rm = TRUE, ...)

p75(y, na_rm = TRUE, ...)

p90(y, na_rm = TRUE, ...)

p95(y, na_rm = TRUE, ...)

p99(y, na_rm = TRUE, ...)
```

## Arguments

- y:

  Object with values

- p:

  Probs as in quantile function

- na_rm:

  Removes NAs

- ...:

  as in quantile function

## Examples

``` r
p25(1:3)
#> 25% 
#> 1.5 
p75(1:4)
#>  75% 
#> 3.25 
p99(0:100)
#> 99% 
#>  99 
```
