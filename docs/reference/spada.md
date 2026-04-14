# Launch a 'shiny' application for data analysis

Generates a 'shiny' application for interactive data analysis.

## Usage

``` r
spada(..., run_local = TRUE)
```

## Arguments

- ...:

  Objects of data.frame class

- run_local:

  Logical. Whether to run the application locally.

## Value

An object of class 'shiny.appobj' representing the 'shiny' application.
Printing the object launches the interactive app in a web browser.

## Examples

``` r
if(interactive()) spada(datasets::mtcars)
```
