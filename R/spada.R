#' Launch a 'shiny' application for data analysis
#'
#' @description Generates a 'shiny' application for interactive data analysis.
#'
#' @param ... Objects of data.frame class
#' @param run_local Logical. Whether to run the application locally. If TRUE the app will close at session end.
#'
#' @examples
#' if(interactive()) spada(datasets::mtcars)
#'
#' @return An object of class 'shiny.appobj' representing the 'shiny' application.
#'   Printing the object launches the interactive app in a web browser.
#'
#' @export
#'
#' @import shiny
#'
#' @importFrom bsicons bs_icon
#'
#' @importFrom bslib accordion accordion_panel bind_task_button bs_add_rules
#'             bs_theme card card_body card_footer card_header font_collection
#'             input_switch layout_column_wrap layout_columns layout_sidebar
#'             navbar_options nav_item nav_menu nav_panel nav_select nav_spacer
#'             navset_card_pill page_navbar popover sidebar tooltip update_switch
#'             value_box
#'
#' @importFrom collapse allNA ffirst flast fmean fmedian fmax fmin fmode fncol
#'             fnrow fnunique fquantile fsd fsum fvar frange qtab whichNA %==%
#'
#' @importFrom data.table %between% %notin% .SD := as.data.table between copy
#'             data.table fcase fifelse fread fwrite hour is.data.table mday melt
#'             minute month quarter rbindlist second setcolorder setDT setnames
#'             setorderv shift wday week yday year
#'
#' @importFrom DescTools Freq Gmean Hmean Kurt Mode Outlier ShapiroFranciaTest
#'             Skew ZTest
#'
#' @importFrom ggplot2 .data aes after_stat coord_flip element_blank
#'             element_rect element_text geom_bar geom_boxplot geom_histogram
#'             geom_hline geom_line geom_point geom_text geom_tile geom_vline
#'             ggplot labs scale_fill_gradient2 stat_boxplot stat_function
#'             stat_qq stat_qq_line theme theme_classic ylim
#'
#' @importFrom graphics abline barplot boxplot curve hist lines mtext polygon text
#'
#' @importFrom grDevices colors
#'
#' @importFrom gt as_raw_html cols_align cols_hide cols_label cols_merge cols_move
#'             cols_width data_color fmt_bytes fmt_icon fmt_integer
#'             fmt_number fmt_percent gt gt_output gtsave opt_interactive pct
#'             render_gt sub_missing sub_values tab_header tab_options tab_spanner
#'
#' @importFrom haven as_factor is.labelled read_sav write_sav
#'
#' @importFrom htmltools plotTag save_html
#'
#' @importFrom mirai daemons mirai
#'
#' @importFrom qs2 qs_read qs_save
#'
#' @importFrom rlang parse_expr
#'
#' @importFrom shinyWidgets colorPickr dropdownButton show_toast radioGroupButtons
#'             statiCard updateColorPickr updateRadioGroupButtons
#'
#' @importFrom stats cor cor.test dnorm formula IQR dnorm ks.test median lm pf
#'             qnorm qqline qqnorm rnorm sd setNames shapiro.test var
#'
#' @importFrom tools file_ext R_user_dir toTitleCase
#'
#' @importFrom utils object.size head packageDescription sessionInfo
#'
#' @importFrom waiter useWaiter waiter_hide waiter_show waiterShowOnLoad
#'
#' @importFrom writexl write_xlsx

spada <- function(..., run_local = TRUE) {

  run_local <- !isFALSE(run_local)
  daemons(1)

  datasets <- list(...)
  if(length(datasets) == 0){
    datasets <- list('iris' = datasets::iris, 'mtcars' = datasets::mtcars)
    no_input_data <- TRUE
  } else{
    no_input_data <- FALSE
  }

  stopifnot('Objects must be data.frame and have at least 1 row and 1 col each' =
              sapply(datasets, is_spada_df) |> all())

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

  # make valid cols --------------------------
  datasets <- lapply(
    datasets,
    \(df) {
      lapply(df, make_valid_cols) |> as.data.table()
    }
  )

  # read conf values ----------------------------------------------------------
  spada_conf_dir <- spada_user_dir('config')
  spada_data_dir <- spada_user_dir('data')

  start_conf <- c(
    'no_input_data' = no_input_data,
    'conf_dir' = spada_conf_dir,
    'data_dir' = spada_data_dir,
    default_conf
  )

  start_conf <- load_conf(start_conf, spada_conf_dir, themes_names)

  # resources -----------------------------------------------------------------
  addResourcePath('spada', system.file('www', package = 'spada'))

  ### Run App -----------------------------------------------------------------
  shinyApp(spada_ui(start_conf),
           spada_server(datasets, start_conf, run_local),
           options = list(launch.browser = T))
}
