
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spada (<u>**S**</u>hiny <u>**Pa**</u>ckage for <u>**D**</u>ata <u>**A**</u>nalysis)

<!-- badges: start -->
<!-- badges: end -->

The goal of **spada** is to provide visual tools for Data Analysis in a
Shiny App.

This package is inspired in many other tools like:

- IBM SPSS Statistics (<https://www.ibm.com/products/spss-statistics>)

- R Commander package
  (<https://cran.r-project.org/web/packages/Rcmdr/index.html>)

- Jamovi (<https://www.jamovi.org/>)

- ydata profiling (<https://docs.profiling.ydata.ai/latest/>)

## Installation

You can install the development version of spada from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lgschuck/spada")
```

## Loading the package

``` r
library(spada)
```

## Usage

``` r
if(interactive()){
  spada()
}
```

## Screenshots

### Data

![](man/figures/spada_home.png)

![](man/figures/spada_home2.png)

![](man/figures/spada_home3.png)

### Edit

![](man/figures/spada_edit_filter.png)

![](man/figures/spada_edit_convert.png)

![](man/figures/spada_edit_order.png)

### Analysis

![](man/figures/spada_analysis_exploratory.png)

### Options

![](man/figures/spada_options_config.png)
