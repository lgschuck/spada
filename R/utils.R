
# list of filters -------------------------------------------------------------
filter_operators <- c('== (Equal)' = '==',
                      '!= (Not Equal)' = '!=',
                      '> (Greater)' = '>',
                      '>= (Greater or Equal)' = '>=',
                      '< (Less)' = '<',
                      '<= (Less or Equal)' = '<=',
                      'Is NA (is.na)' = 'is_na',
                      'Not NA (! is.na)' = 'not_na',
                      'In (%in%)' = 'in',
                      'Not In (! %in%)' = 'not_in',
                      'Between' = 'between',
                      'Not Between' = 'not_between')

# lits of date formats --------------------------------------------------------
date_formats <- c(
  # hifen
  'YYYY-MM-DD' = '%Y-%m-%d',
  'DD-MM-YYY' = '%d-%m-%Y',
  'MM-DD-YY' = '%m-%d-%y',
  'YYYY-MMM-DD' = '%Y-%b-%d',
  'YY-MMM-DD' = '%y-%b-%d',
  'YY-MM-DD' = '%y-%m-%d',
  # bars
  'YYYY/MM/DD' = '%Y/%m/%d',
  'DD/MM/YYYY' = '%d/%m/%Y',
  'MM/DD/YY' = '%m/%d/%y',
  'YYYY/MMM/DD' = '%Y/%b/%d',
  'YY/MMM/DD' = '%y/%b/%d',
  'YY/MM/DD' = '%y/%m/%d',
  # dots
  'YYYY.MM.DD' = '%Y.%m.%d',
  'DD.MM.YYYY' = '%d.%m.%Y',
  'MM.DD.YY' = '%m.%d.%y',
  'YYYY.MMM.DD' = '%Y.%b.%d',
  'YY.MMM.DD' = '%y.%b.%d',
  'YY.MM.DD' = '%y.%m.%d',
  # no separator
  'YYYYMMDD' = '%Y%m%d',
  'DDMMYYYY' = '%d%m%Y',
  'MMDDYY' = '%m%d%y',
  'YYYYMMMDD' = '%Y%b%d',
  'YYMMMDD' = '%y%b%d',
  'YYMMDD' = '%y%m%d'
)

# close browser tab -----------------------------------------------------------
js_exit <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"

tag_js_exit <- tags$head(tags$script(HTML(js_exit)))

# css ---------------------------------------------------------------------

tag_css <- tags$head(tags$style(HTML(
  "
    /* change color of navbar */
    .navbar {
      /*background: linear-gradient(to right, #1d3f52, #033854, #02517d, #317aa3, #008080);*/
      /*background: linear-gradient(to right, #0277bd, #0277bd);*/
      background: #007bb5;
    }

    /* change size of nav panel */
    .nav-link {font-size: 17px; }

    body { font-size: 0.9rem; }

    .card {
      border-radius: 0rem;
      margin: -8px;
    }

    .mini-header {
      color: white;
      /*background: linear-gradient(to right, #1d3f52, #033854, #02517d, #317aa3, #20adc9, #008080);*/
      /*background: linear-gradient(to right, #1f4e72, #2a6485, #3a7f9d, #4b97b6, #63a9ca, #7bbfce);*/
      background: linear-gradient(to right, #1f4e72, #2a6485, #3a7f9d, #4b97b6, #5fa3c2, #4e96b6);
    }

    .btn-task {
      color: #0072B2;
      background-color: #f9f9f9;
      border-color: #0072B2;
    }

    .btn-task:hover {
      background-color: #0072B2;
      border-color: #0072B2;
      color: white;
    }

    /* border rectangular */

    .card, .well {
     --bs-card-inner-border-radius: 0;
    }

    .card-body {border-radius: 0rem;}

    .nav-pills {
      --bs-nav-pills-border-radius: 0rem;
      --bs-nav-pills-link-active-color: #02517d;
      /*--bs-nav-pills-link-active-bg: #d5d6d7;*/
      --bs-nav-pills-link-active-bg: #e3e3e4;
    }

    .value-box-title {
      font-size: 1rem !important;
    }

    .value-box-value {
      font-size: 1.5rem !important;
    }

    }

  "))
)

# info about dataset variables -----------------------------------------------
df_info <- function(df){
  rows <- nrow(df)
  n_nas = sapply(df, \(x) suna(is.na(x)))
  n_valid <- rows - n_nas
  n_unique <- sapply(df, \(x) length(unique(x)))
  n_zero <- sapply(df, \(x) suna(x == 0))

  data.frame(
    var = names(df),
    type = sapply(df, typeof),
    class = sapply(df, \(x) paste(class(x), collapse = "/")),
    size = sapply(df, object.size),
    min = sapply(df, \(x) if (is.numeric(x)) mina(x) else NA),
    max = sapply(df, \(x) if (is.numeric(x)) mana(x) else NA),
    n_valid = n_valid,
    perc_valid = n_valid / rows,
    n_unique = n_unique,
    perc_unique = n_unique / rows,
    n_zero = n_zero,
    perc_zero = n_zero / rows,
    n_nas = n_nas,
    perc_nas = n_nas / rows
  )
}

# empty plot function ---------------------------------------------------------
empty_plot <- function(msg = 'No plot', c = 2){
  plot(1:10, 1:10, type = 'n', xlab = '', ylab = '')
  text(5, 5, msg, cex = c)
}

# bslib btn task --------------------------------------------------------------
btn_task <- function(ID, LABEL, ICON = NULL){
  bslib::input_task_button(id = ID, label = LABEL, icon = ICON, class = 'btn-task')
}

# messages - shownotification -------------------------------------------------
msg <- function(TEXT, DURATION = 2){
  showNotification(ui = TEXT, duration = DURATION, type = 'message')
}

msg_error <- function(TEXT, DURATION = 2){
  showNotification(ui = TEXT, duration = DURATION, type = 'error')
}

# is date function ------------------------------------------------------------
is_date <- function(x){
  inherits(x, c('Date', 'POSIXt', 'POSIXct', 'POSIXlt'))
}

# try convert -----------------------------------------------------------------
try_convert <- function(x, fun){
  tryCatch(fun(x),
           error = function(e) rep(NA, x |> length()),
           warning = function(w) rep(NA, x |> length()))
}

# format conversion ----------------------------------------------------------
convert <- function(x, type, date_format = '%Y-%m-%d', date_origin = '1970-01-01'){
  if(x |> is.raw()) x <- as.numeric(x)

  if(type == 'as.numeric'){
    if(x |> is.character()) rep(NA, length(x)) else as.numeric(x)
  } else if(type == 'as.integer'){
    if(x |> is.character()) rep(NA, length(x)) else as.integer(x)
  } else if(type == 'as.character'){
    as.character(x)
  } else if(type == 'as.Date'){
    if(is.numeric(x)){
      as.Date(x, origin = date_origin)
    } else if (is.raw(x) || is.complex(x)){
      as.Date(x |> as.numeric(), origin = date_origin)
    } else as.Date(x, format = date_format)
  } else if(type == 'as.factor'){
    as.factor(x)
  } else if(type == 'as.double'){
    if(x |> is.character()) rep(NA, length(x)) else as.double(x)
    # } else if(type == 'as.raw'){
    #   as.raw(x)
  } else if(type == 'as.complex'){
    if(x |> is.character()) rep(NA, length(x)) else as.complex(x)
  }
}

# valid name ------------------------------------------------------------------
is_valid_name <- function(x){
  x == make.names(x)
}

# gt info ---------------------------------------------------------------------

gt_info <- function(df){
  setDT(df)
  df[, f_icon := fcase(
    class == 'integer', '1',
    class == 'character', 'a',
    class == 'numeric', 'calculator',
    class %in% c('Date', 'POSIXt', 'POSIXct', 'POSIXlt',
                 'POSIXct/POSIXt', 'POSIXlt/POSIXt'), 'calendar',
    class == 'factor', 'sitemap',
    class == 'raw', 'sd-card',
    class == 'complex', 'info')]

  df |>
    gt::gt() |>
    gt::fmt_percent(columns = c('perc_valid', 'perc_unique', 'perc_zero', 'perc_nas')) |>
    gt::fmt_bytes(columns = 'size') |>
    gt::data_color(columns = 'size', palette = blue_palette) |>
    gt::data_color(columns = 'min', palette = yl_palette) |>
    gt::data_color(columns = 'max', palette = pk_palette) |>
    gt::data_color(columns = 'n_valid', palette = lg_palette) |>
    gt::data_color(columns = 'n_unique', palette = dg_palette) |>
    gt::data_color(columns = 'n_zero', palette = gray_palette) |>
    gt::data_color(columns = 'n_nas', palette = red_palette) |>
    gt::fmt_integer(columns = c('n_valid', 'n_unique', 'n_nas')) |>
    gt::fmt_number(columns = c('min', 'max', 'n_valid', 'n_unique', 'n_zero', 'n_nas')) |>
    gt::sub_missing() |>
    gt::opt_interactive(
      use_filters = T,
      use_resizers = T,
      use_highlight = T,
      use_compact_mode = T,
      use_text_wrapping = F,
      use_page_size_select = T
    ) |>
    gt::cols_move(columns = 'f_icon', after = 'var') |>
    gt::fmt_icon(columns = f_icon) |>
    gt::cols_label(
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
    gt::cols_width(f_icon ~ px(30)) |>
    gt::cols_merge(columns = c(class, f_icon), pattern = "{2} {1}") |>
    gt::cols_merge(columns = c(n_valid, perc_valid), pattern = "{1} / {2}") |>
    gt::cols_merge(columns = c(n_unique, perc_unique), pattern = "{1} / {2}") |>
    gt::cols_merge(columns = c(n_zero, perc_zero), pattern = "{1} / {2}") |>
    gt::cols_merge(columns = c(n_nas, perc_nas), pattern = "{1} / {2}") |>
    gt::tab_options(table.background.color = '#ffffff')
}

# palettes --------------------------------------------------------------------

gray_palette <- c('#ffffff', '#585858', '#232323')
blue_palette <- c('#ffffff', '#096691', '#134359')
yl_palette <- c('#ffffff', '#ffc107', '#f7a305')
dg_palette <- c('#ffffff','#1c6561', '#284e4c')
lg_palette <- c('#ffffff', '#0cb0a8', '#09918b')
pk_palette <- c('#ffffff', '#bf007f', '#8f0360')
red_palette <- c('#ffffff', '#b60020', '#750217')
