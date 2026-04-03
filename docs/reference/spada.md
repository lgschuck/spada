# Spada (Data Analysis)

Function that generates a Shiny App for Data Analysis

## Usage

``` r
spada(..., run_local = TRUE)
```

## Arguments

- ...:

  Objects of data.frame class

- run_local:

  Run on local machine

## Examples

``` r
if(interactive()) spada(datasets::mtcars)
```
