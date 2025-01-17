
#' Show in interactive gt table a data.frame from df_info function
#'
#' @param df A data.frame object from df_info function
#'
#' @examples
#' mtcars |> df_info() |> gt_info()
#'
#' @export
#'
#' @importFrom data.table setDT
#' @importFrom gt cols_hide cols_label cols_merge cols_move cols_width data_color
#'             fmt_bytes fmt_icon fmt_integer fmt_number fmt_percent gt
#'             gt_output opt_interactive render_gt sub_missing tab_options

gt_info <- function(df){
  stopifnot(is.data.frame(df))

  setDT(df)
  df[, f_icon := fcase(
    class == 'integer', '1',
    class == 'character', 'a',
    class == 'numeric', 'calculator',
    class %in% c('Date', 'POSIXt', 'POSIXct', 'POSIXlt',
                 'POSIXct/POSIXt', 'POSIXlt/POSIXt'), 'calendar',
    class == 'factor', 'sitemap',
    class == 'raw', 'sd-card',
    class == 'complex', 'info',
    class == 'logical', 'puzzle-piece')]

  df_gt <- df[, `:=` (n_valid = f_num(n_valid, dig = 3),
                      n_unique = f_num(n_unique, dig = 3),
                      n_zero = f_num(n_zero, dig = 3),
                      n_nas = f_num(n_nas, dig = 3))] |>
    gt()

  # if all NA do nothing
  if(!all(df$min |> is.na())){
    df_gt <- df_gt |>
      data_color(columns = 'min', palette = yl_palette)
  }

  # if all NA do nothing
  if(!all(df$max |> is.na())){
    df_gt <- df_gt |>
      data_color(columns = 'max', palette = pk_palette)
  }

  df_gt |>
    cols_hide(columns = c('rows', 'cols')) |>
    fmt_percent(columns = c('perc_valid', 'perc_unique', 'perc_zero', 'perc_nas')) |>
    fmt_bytes(columns = 'size') |>
    data_color(columns = 'size', palette = blue_palette) |>
    data_color(columns = 'n_valid', palette = lg_palette) |>
    data_color(columns = 'n_unique', palette = dg_palette) |>
    data_color(columns = 'n_zero', palette = gray_palette) |>
    data_color(columns = 'n_nas', palette = red_palette) |>
    fmt_integer(columns = c('n_valid', 'n_unique', 'n_nas')) |>
    fmt_number(columns = c('min', 'max', 'n_valid', 'n_unique', 'n_zero', 'n_nas')) |>
    sub_missing() |>
    opt_interactive(
      use_filters = T,
      use_resizers = T,
      use_highlight = T,
      use_compact_mode = T,
      use_text_wrapping = F,
      use_page_size_select = T
    ) |>
    cols_move(columns = 'f_icon', after = 'var') |>
    fmt_icon(columns = f_icon) |>
    cols_label(
      var = 'Variable',
      type = 'Type',
      class = 'Class',
      size = 'Size',
      min = 'Min',
      max = 'Max',
      n_valid = 'Valid',
      n_unique = 'Unique',
      n_zero = 'Zeros',
      n_nas = "NA's",
      f_icon = ''
    ) |>
    cols_width(f_icon ~ px(30)) |>
    cols_merge(columns = c(class, f_icon), pattern = "{2} {1}") |>
    cols_merge(columns = c(n_valid, perc_valid), pattern = "{1} / {2}") |>
    cols_merge(columns = c(n_unique, perc_unique), pattern = "{1} / {2}") |>
    cols_merge(columns = c(n_zero, perc_zero), pattern = "{1} / {2}") |>
    cols_merge(columns = c(n_nas, perc_nas), pattern = "{1} / {2}") |>
    tab_options(table.background.color = '#ffffff')
}
