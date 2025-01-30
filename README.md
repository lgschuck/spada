
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spada (<u>**S**</u>hiny <u>**Pa**</u>ckage for <u>**D**</u>ata <u>**A**</u>nalysis) <img src="man/figures/logo.png" align="right" height="139"/>

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

<div style="border: 2px solid #02517d; padding: 10px; background-color: #a9ccdb;">

<strong>Warning:</strong> Spada is in active development.

</div>

## Installation

You can install the development version of spada from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lgschuck/spada")
```

For a specific release visit
[Releases](https://github.com/lgschuck/spada/releases) and change the
ref parameter bellow for the tag name:

![](man/figures/install_release.png)

``` r
devtools::install_github("lgschuck/spada", ref = "2025.01.13-1")
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
