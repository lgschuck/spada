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
#'
#' @importFrom bsicons bs_icon
#'
#' @importFrom bslib accordion accordion_panel bs_add_rules bs_theme card card_body
#'             card_footer card_header layout_column_wrap layout_columns
#'             layout_sidebar nav_item nav_menu nav_panel nav_select nav_spacer
#'             navset_card_pill page_navbar popover sidebar tooltip value_box
#'
#' @importFrom data.table %between% %notin% .SD := as.data.table between copy
#'             fcase fifelse fread fwrite hour mday minute month quarter second
#'             setcolorder setDT setnames setorderv shift wday week yday year

#' @importFrom DescTools Gmean Hmean Kurt Mode Outlier ShapiroFranciaTest
#'             Skew ZTest
#'
#' @importFrom dplyr arrange if_else filter mutate pull select
#'
#' @importFrom ggplot2 .data aes after_stat coord_flip element_blank
#'             element_rect element_text geom_bar geom_boxplot geom_histogram
#'             geom_hline geom_line geom_point geom_vline ggplot labs
#'             stat_boxplot stat_function stat_qq stat_qq_line theme
#'             theme_classic ylim
#'
#' @importFrom graphics abline barplot boxplot curve hist lines mtext polygon text
#'
#' @importFrom grDevices colors
#'
#' @importFrom gt cols_align cols_hide cols_label cols_merge cols_move
#'             cols_width data_color fmt_bytes fmt_icon fmt_integer
#'             fmt_number fmt_percent gt gt_output gtsave opt_interactive
#'             render_gt sub_missing sub_values tab_header tab_options tab_spanner
#'
#' @importFrom haven as_factor is.labelled read_sav write_sav
#'
#' @importFrom htmltools plotTag save_html
#'
#' @importFrom rlang parse_expr
#'
#' @importFrom sass as_sass
#'
#' @importFrom shinyWidgets colorPickr updateColorPickr show_toast dropdownButton
#'             radioGroupButtons statiCard updateRadioGroupButtons
#'
#' @importFrom shinybusy busy_start_up show_modal_progress_line spin_epic
#'             update_modal_progress
#'
#' @importFrom stats cor cor.test dnorm formula IQR dnorm ks.test median lm
#'             qnorm qqline qqnorm rnorm sd shapiro.test var
#'
#' @importFrom tools R_user_dir toTitleCase
#'
#' @importFrom utils object.size head packageDescription sessionInfo

spada <- function(...) {
  datasets <- list(...)
  if(length(datasets) == 0) datasets <- list('iris' = datasets::iris,
                                             'mtcars' = datasets::mtcars)
  stopifnot('Objects must be data.frame and have at least 1 row each' =
    sapply(datasets, is.data.frame) |> all() && all(sapply(datasets, nrow) > 0))

  # set datasets names
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

  # make sure all datasets variables are valid names
  datasets <- lapply(datasets, make_var_names)

  # read conf values ----------------------------------------------------------
  r_user_conf_dir <- normalizePath(R_user_dir('spada', 'config'), winslash = '/', mustWork = F)
  r_user_data_dir <- normalizePath(R_user_dir('spada', 'data'), winslash = '/', mustWork = F)

  if(!dir.exists(r_user_conf_dir)) {
    dir.create(r_user_conf_dir, recursive = T)
  }

  if(!dir.exists(r_user_data_dir)) {
    dir.create(r_user_data_dir, recursive = T)
  }

  temp_conf <- list(
    'conf_dir' = r_user_conf_dir,
    'data_dir' = r_user_data_dir,
    'theme' = 'spada_theme',
    'file_size' = 1000,
    'restore_session' = 'always',
    'save_session' = 'always'
  )

  if(file.exists(paste0(r_user_conf_dir, '/conf.RDS'))) {
    temp_conf_saved <- readRDS(paste0(r_user_conf_dir, '/conf.RDS'))

    temp_conf$theme <- temp_conf_saved$theme
    temp_conf$file_size <- temp_conf_saved$file_size
    temp_conf$restore_session <- temp_conf_saved$restore_session
    temp_conf$save_session <- temp_conf_saved$save_session
  } else {
    saveRDS(temp_conf,
            paste0(r_user_conf_dir, '/conf.RDS'),
            compress = F)
  }

  ### Run App -----------------------------------------------------------------
  shinyApp(spada_ui(temp_conf), spada_server(datasets, temp_conf),
           options = list(launch.browser = T))
}
