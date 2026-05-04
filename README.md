
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spada (<u>**S**</u>hiny <u>**Pa**</u>ckage for <u>**D**</u>ata <u>**A**</u>nalysis) <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/spada)](https://cran.r-project.org/package=spada)

[![](https://cranlogs.r-pkg.org/badges/spada)](https://cran.rstudio.com/web/packages/spada/index.html)

<!-- badges: end -->

The goal of **spada** is to provide visual tools for Data Analysis in a
Shiny App.

This package is inspired in many other tools like:

- IBM SPSS Statistics (<https://www.ibm.com/products/spss-statistics>)

- R Commander package (<https://CRAN.R-project.org/package=Rcmdr>)

- Jamovi (<https://www.jamovi.org/>)

- ydata profiling (<https://docs.profiling.ydata.ai/latest/>)

<div style="border: 2px solid #02517d; padding: 10px; background-color: #a9ccdb;">

<strong>Warning:</strong> Spada is in active development.

</div>

## [Live Demo](https://lgschuck.shinyapps.io/spada)

You may try Spada in shinyapps.io
[Spada](https://lgschuck.shinyapps.io/spada)

## Documentation

You can access Spada Book on this website: [Spada
Book](https://lgschuck.github.io/spada_book/).

## Installation

``` r
install.packages("spada")
```

You can install the development version of spada from
[GitHub](https://github.com/) using the command below.

``` r
install.packages("remotes")
remotes::install_github(
  "lgschuck/spada",
  dependencies = TRUE
)
```

### Loading the package

``` r
library(spada)
```

### Usage

``` r
if(interactive()){
  spada()
}
```

## Docker

Spada is distributed as a **Docker Image**.

For instructions in how to download and use Spada go to
[Docker](https://lgschuck.github.io/spada_book/docker.html)

You can use [Docker Hub](https://hub.docker.com/r/lgschuck/spada) to
search for diferent versions.

## Screenshots

<figure>
<img src="man/figures/spada_loading_0.1.3.png" alt="Loading" />
<figcaption aria-hidden="true">Loading</figcaption>
</figure>

### Data

<figure>
<img src="man/figures/spada_home.png" alt="Home" />
<figcaption aria-hidden="true">Home</figcaption>
</figure>

<figure>
<img src="man/figures/spada_home2.png" alt="Home 2" />
<figcaption aria-hidden="true">Home 2</figcaption>
</figure>

#### Data \> Metadata

<figure>
<img src="man/figures/spada_data_metadata.png" alt="Metadata" />
<figcaption aria-hidden="true">Metadata</figcaption>
</figure>

#### Data \> Overview

<figure>
<img src="man/figures/spada_data_overview.png" alt="Overview" />
<figcaption aria-hidden="true">Overview</figcaption>
</figure>

### Edit

<figure>
<img src="man/figures/spada_edit.png" alt="Edit" />
<figcaption aria-hidden="true">Edit</figcaption>
</figure>

### Analysis

<figure>
<img src="man/figures/spada_analysis_exploratory.png" alt="Analysis" />
<figcaption aria-hidden="true">Analysis</figcaption>
</figure>

### Output

<figure>
<img src="man/figures/spada_output.png" alt="Output" />
<figcaption aria-hidden="true">Output</figcaption>
</figure>

### Options

<figure>
<img src="man/figures/spada_config.png" alt="Options" />
<figcaption aria-hidden="true">Options</figcaption>
</figure>
