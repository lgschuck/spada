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
#' @import shiny
#' @import data.table
#' @importFrom bsicons bs_icon
#' @importFrom gt cols_align cols_hide cols_label cols_merge cols_move
#'             cols_width data_color fmt_bytes fmt_icon fmt_integer
#'             fmt_number fmt_percent gt gt_output opt_interactive render_gt
#'             sub_missing tab_options
#'
#' @importFrom bslib accordion accordion_panel bs_theme card card_body
#'             card_footer card_header layout_column_wrap layout_columns
#'             layout_sidebar nav_item nav_menu nav_panel nav_select nav_spacer
#'             navset_card_pill page_navbar popover sidebar tooltip value_box
#' @importFrom shinyWidgets colorPickr updateColorPickr
#' @importFrom dplyr arrange filter mutate pull select
#' @importFrom graphics abline hist
#' @importFrom utils object.size head
#' @importFrom graphics barplot boxplot curve lines mtext text
#' @importFrom stats cor lm sd var median rnorm IQR cor.test dnorm ks.test
#'             qqline qqnorm shapiro.test
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
