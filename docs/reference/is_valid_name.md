# Test if values are valid names

Test if values are valid names

## Usage

``` r
is_valid_name(x)
```

## Arguments

- x:

  values to test

## Examples

``` r
'abc ' |> is_valid_name()
#> [1] FALSE
'abc' |> is_valid_name()
#> [1] TRUE
c(1, 'a', 'b') |> is_valid_name()
#> [1] FALSE  TRUE  TRUE
list('1', 2, 3, NA, 'a') |> is_valid_name()
#> [1] FALSE FALSE FALSE FALSE  TRUE
```
