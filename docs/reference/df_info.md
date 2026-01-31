# Info about dataset variables

Info about dataset variables

## Usage

``` r
df_info(df)
```

## Arguments

- df:

  A data.frame object

## Examples

``` r
df_info(mtcars)
#>        var   type   class  size    min     max n_valid perc_valid n_unique
#>     <char> <char>  <char> <num>  <num>   <num>   <int>      <num>    <int>
#>  1:    mpg double numeric   304 10.400  33.900      32          1       25
#>  2:    cyl double numeric   304  4.000   8.000      32          1        3
#>  3:   disp double numeric   304 71.100 472.000      32          1       27
#>  4:     hp double numeric   304 52.000 335.000      32          1       22
#>  5:   drat double numeric   304  2.760   4.930      32          1       22
#>  6:     wt double numeric   304  1.513   5.424      32          1       29
#>  7:   qsec double numeric   304 14.500  22.900      32          1       30
#>  8:     vs double numeric   304  0.000   1.000      32          1        2
#>  9:     am double numeric   304  0.000   1.000      32          1        2
#> 10:   gear double numeric   304  3.000   5.000      32          1        3
#> 11:   carb double numeric   304  1.000   8.000      32          1        6
#>     perc_unique n_zero perc_zero n_nas perc_nas  rows  cols
#>           <num>  <int>     <num> <int>    <num> <int> <int>
#>  1:     0.78125      0   0.00000     0        0    32    11
#>  2:     0.09375      0   0.00000     0        0    32    11
#>  3:     0.84375      0   0.00000     0        0    32    11
#>  4:     0.68750      0   0.00000     0        0    32    11
#>  5:     0.68750      0   0.00000     0        0    32    11
#>  6:     0.90625      0   0.00000     0        0    32    11
#>  7:     0.93750      0   0.00000     0        0    32    11
#>  8:     0.06250     18   0.56250     0        0    32    11
#>  9:     0.06250     19   0.59375     0        0    32    11
#> 10:     0.09375      0   0.00000     0        0    32    11
#> 11:     0.18750      0   0.00000     0        0    32    11
```
