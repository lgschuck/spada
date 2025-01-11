#' Spada (Data Analysis)
#'
#' Function that generates a Shiny App for Data Analysis
#'
#' @param ... Objects of data.frame class
#'
#' @examples
#' if(interactive()) spada(datasets::mtcars)
#'
#' @export
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#'
#' @import data.table
#' @importFrom dplyr arrange filter mutate pull select
#' @import bslib
#' @import bsicons
#' @import DT
#' @import gt
#' @importFrom graphics abline hist
#' @importFrom stats median
#' @importFrom utils object.size head
#' @importFrom graphics boxplot lines barplot mtext text
#' @importFrom stats cor lm sd var rnorm IQR cor.test
#' @importFrom grDevices colors

spada <- function(...) {
  datasets <- list(...)
  if(length(datasets) == 0) datasets <- list('iris' = datasets::iris, 'mtcars' = datasets::mtcars)
  stopifnot('Objects must be data.frame and have at least 1 row each' =
    sapply(datasets, is.data.frame) |> all() && all(sapply(datasets, nrow) > 0))

  # set names
  if(is.null(names(datasets))){
    names(datasets) <- lapply(substitute(list(...))[-1], deparse) |>
      unlist() |>
      make.names(unique = T)
  } else {
    new_names <- paste0('dataset_', 1:length(datasets))
    empty_names <- which(names(datasets) == '')
    names(datasets)[empty_names] <- new_names[empty_names]

    names(datasets) <- make.names(names(datasets), unique = T)
  }
  gc()

  ### Run App -----------------------------------------------------------------
  shinyApp(spada_ui(), spada_server(datasets),
           options = list(launch.browser = T))
}
