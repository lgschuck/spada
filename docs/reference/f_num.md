# Format number with abreviation and separators

Return is character class

## Usage

``` r
f_num(
  x,
  big = ",",
  dec = ".",
  thousand = "K",
  million = "M",
  billion = "B",
  dig = 0
)
```

## Arguments

- x:

  Object with values

- big:

  Thousand separator

- dec:

  Decimal separator

- thousand:

  Abreviation for thousands

- million:

  Abreviation for millions

- billion:

  Abreviation for billions

- dig:

  Digits after decimal mark

## Examples

``` r
f_num(12345678956, billion = 'G')
#> [1] "12 G"
f_num(512347896, million = 'Mi')
#> [1] "512 Mi"
f_num(9995198, thousand = 'm', dig = 3, dec = ',', big = '.')
#> [1] "9,995 M"
f_num(55566312345678956, billion = 'G')
#> [1] "55,566,312 G"
f_num(Inf)
#> [1] "Inf"
f_num(-Inf)
#> [1] "-Inf"
```
