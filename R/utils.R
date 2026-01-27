
# =============================================================================
# ---------------------------- OBJECTS ----------------------------------------
# =============================================================================

# default conf ----------------------------------------------------------------
default_conf <- list(
  'theme' = 'spada_theme',
  'file_size' = 1000,
  'restore_session' = 'never',
  'save_session' = 'ask',
  'restore_data_status' = 0,
  'restore_output_status' = 0,
  'restore_status' = NULL,
  'plot_fill_color' = plot_fill_color,
  'plot_line_color' = plot_line_color,
  'plot_title_color' = plot_title_color,
  'plot_limit' = 1e5
)

# math functions --------------------------------------------------------------
math_funs <- c(
  c('Mean' = 'mean',
    'Geometric Mean' = 'Gmean',
    'Harmonic Mean' = 'Hmean',
    # 'Mode' = 'Mode',
    'Standard Deviation' = 'sd',
    'Variance' = 'var',
    'Min' = 'min',
    'Mina' = 'mina',
    'Max' = 'max',
    'Mana' = 'mana',
    'First' = 'fina',
    'Last' = 'lana',
    'Lag' = 'shift',
    # 'Range' = 'range',
    'IQR' = 'IQR',
    'Skewness' = 'Skew',
    'Kurtosis' = 'Kurt',
    'Sum' = 'sum',
    'Suna' = 'suna',
    'Cum Sum' = 'cumsum',
    'Prod' = 'prod',
    'Cum Prod' = 'cumprod',
    'Square Root' = 'sqrt',
    'Exponential' = 'exp',
    'Log' = 'log',
    'Log2' = 'log2',
    'Log10' = 'log10',
    'Ceiling' = 'ceiling',
    'Floor' = 'floor',
    'Trunc' = 'trunc',
    'Signif' = 'signif',
    'Round' = 'round',
    'Abs' = 'abs',
    'Sine' = 'sin',
    'Cosine' = 'cos',
    'Tangent' = 'tan',
    'Order' = 'order',
    'As Numeric' = 'as.numeric',
    'Is Numeric' = 'is.numeric',
    'As Double' = 'as.double',
    'Is Double' = 'is.double',
    'As Integer' = 'as.integer',
    'Is Integer' = 'is.integer',
    'Is Integer' = 'is.integer',
    'As Numeric' = 'as.numeric',
    'Is Numeric' = 'is.numeric'
    )
)

# char functions --------------------------------------------------------------
char_funs <- c(
  'To Upper' = 'toupper',
  'To Lower' = 'tolower',
  'N Char' = 'nchar',
  'As Char' = 'as.character',
  'Is Char' = 'is.character'
)

# date functions --------------------------------------------------------------
date_funs <- c(
  # data.table package
  'Year' = 'year',
  'Month' = 'month',
  'Month Day' = 'mday',
  'Week Day' = 'wday',
  'Year Day' = 'yday',
  'Week' = 'week',
  'Quarter' = 'quarter',
  'Hour' = 'hour',
  'Minute' = 'minute',
  'Second' = 'second',
  # spada package
  'Is Date' = 'is_date'
)

# factor functions ------------------------------------------------------------
factor_funs <- c(
  'Number of Levels' = 'nlevels',
  'As factor' = 'as.factor',
  'Is Factor' = 'is.factor',
  'Is Ordered' = 'is.ordered'
)

# logical functions -----------------------------------------------------------
logical_funs <- c(
  'All True' = 'all',
  'Any True' = 'any',
  'Is Logical' = 'is.logical',
  'Number of True' = 'sum',
  'Proportion of True' = 'mean'
)

# complex functions -----------------------------------------------------------
complex_funs <- c(
  'Real Part' = 'Re',
  'Imaginary Part' = 'Im',
  'Is Complex' = 'is.complex'
)

# basic operations ------------------------------------------------------------
basic_operations <- c(
  # base package
  '+', '-', '+', '-', '/', '*', '^',
  '==', '!=', '!', '>', '>=', '<', '<=',
  '&', '|', 'xor',
  '(', '[', '<-', '$',
  '%in%', 'is.na', 'is.null', 'is.nan', 'na.omit',
  '::', ':', 'T', 'F',
  'list', 'try',
  'paste', 'paste0', 'substr',
  'isTRUE', 'isFALSE',
  'c',
  'as.Date', 'as.POSIXct', 'as.POSIXlt',
  'ifelse',

  # data.table package
  '%notin%', 'between', '%between%', 'fifelse', 'fcase',

  # DescTools package
  'Outlier',

  # dplyr package
  'if_else',

  #stats package
  'quantile',

  #spada package
  'fina', 'lana', 'mana', 'mina', 'suna'
)

# dangerous operations --------------------------------------------------------
dangerous_operations <- c(
  # R base - code
  'assign', 'attach',
  'do.call',
  'eval', 'eval.parent', 'evalq',
  'get', 'globalenv', 'new.env', 'parent.env', 'parse',
  'remove', 'rm', 'setwd', 'substitute', 'Sys.setenv',

  # R base - files system
  'dir.create', 'dir.exists', 'dir.remove',
  'file.copy', 'file.create', 'file.remove', 'file.rename',
  'load', 'save', 'saveRDS', 'unlink',

  # R base - network
  'curl', 'download.file', 'httr::DELETE', 'httr::GET', 'httr::POST', 'httr::PUT',
  'RCurl::getURL', 'RCurl::postForm', 'socketConnection', 'url',

  # R base - IO
  'read.csv', 'readLines', 'read.table', 'readRDS',
  'write.csv', 'writeLines', 'write.table',

  # R base system
  'shell', 'shell.exec', 'system', 'system2', 'gc',

  # data.table
  ':=',
  'set', 'setattr', 'setcolorder', 'setnames', 'setDT',
  'setDF', 'setkey', 'setorder', 'substitute2'
)

# allowed operations ----------------------------------------------------------
allowed_operations <- c(
  basic_operations,
  math_funs,
  char_funs,
  date_funs,
  factor_funs,
  logical_funs
) |> unique() |> sort()

# list of filters -------------------------------------------------------------
equal_operators <- c('== (Equal)' = '==',
                     '!= (Not Equal)' = '!=')

compare_operators <- c('> (Greater)' = '>',
                       '>= (Greater or Equal)' = '>=',
                       '< (Less)' = '<',
                       '<= (Less or Equal)' = '<=')

na_operators <- c('Is NA (is.na)' = 'is_na',
                  'Not NA (! is.na)' = 'not_na')

in_operators <- c('In (%in%)' = 'in',
                  'Not In (! %in%)' = 'not_in')

between_operators <- c('Between' = 'between',
                       'Not Between' = 'not_between')

outlier_operators <- c('Outlier' = 'outlier',
                       'Not Outlier' = 'not_outlier')

logical_operators <- c('TRUE' = 'is_true',
                       'FALSE' = 'is_false')

filter_operators <- c(
  equal_operators,
  compare_operators,
  na_operators,
  in_operators,
  between_operators,
  outlier_operators,
  logical_operators
)

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

# list of summarise functions -------------------------------------------------
summarise_functions <- c(
  'Distinct' = 'distinct',
  'Count' = 'count'
)

# close browser tab -----------------------------------------------------------
js_exit <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"

tag_js_exit <- tags$head(tags$script(HTML(js_exit)))

# waiter screen ----------------------------------------------------------------
waiter_right_foot <- div('Built with R and Shiny', class = 'screen-footer-right')
waiter_spinner <- div(class = 'screen-spinner')

waiter_screen <- tags$style(
  HTML(
    "
    .waiter-overlay {
      font-family: 'Segoe UI', Ubuntu, system-ui;
      color: white;
      display: flex;
      justify-content: center;
      align-items: center;

      background: linear-gradient(
        55deg,
        #FFFFFF,
        #1A7DBA,
        #146394,
        #0A5A88,
        #02517D,
        #0F172A,
        #0B1220
      );

      background-size: 400% 400%;
      animation: gradientMove 12s ease infinite;
    }

    @keyframes gradientMove {
      0%   { background-position: 0% 50%; }
      50%  { background-position: 100% 50%; }
      100% { background-position: 0% 50%; }
    }

    .waiter-overlay .screen-container {
      text-align: center;
      animation: fadeIn 1.2s ease;
    }

    .waiter-overlay .screen-title {
      font-size: 144px;
      font-weight: 600;
      letter-spacing: 1px;
    }

    .waiter-overlay .screen-subtitle {
      margin-top: 10px;
      font-size: 32px;
      opacity: 0.9;
    }

    .waiter-overlay .screen-subtitle2 {
      margin-top: 5px;
      font-size: 18px;
      opacity: 0.9;
    }

    .waiter-overlay .screen-spinner {
      margin: 30px auto 0;
      width: 60px;
      height: 60px;
      border: 5px solid rgba(255,255,255,0.2);
      border-top: 5px solid white;
      border-radius: 50%;
      animation: spin 1.1s linear infinite;
    }

    .screen-footer-right {
      font-family: 'Segoe UI', Ubuntu, system-ui;
      color: white;
      position: fixed;
      bottom: 12px;
      right: 16px;
      font-size: 14px;
      /*z-index: 9999;*/
    }

    @keyframes spin {
      to { transform: rotate(360deg); }
    }

    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(8px); }
      to   { opacity: 1; transform: translateY(0); }
    }
  "
  )
)

# =============================================================================
# ---------------------------- FUNCTIONS --------------------------------------
# =============================================================================

# load conf -------------------------------------------------------------------

load_conf <- function(start_conf,
                      r_user_conf_dir,
                      themes_names) {

  conf_path <- file.path(r_user_conf_dir, 'conf.qs2')

  if (file.exists(conf_path)) {
    conf_saved <- qs_read(conf_path)

    required_fields <- c(
      'theme',
      'file_size',
      'restore_session',
      'save_session',
      'plot_fill_color',
      'plot_line_color',
      'plot_title_color',
      'plot_limit'
    )

    # if all TRUE copy saved conf
    if (
      # test required items
      is.list(conf_saved) &&
        all(required_fields %in% names(conf_saved)) &&

      # test theme
      length(conf_saved$theme) == 1 &&
        isTRUE(conf_saved$theme %in% themes_names) &&

      # file size
      length(conf_saved$file_size) == 1 &&
        is.numeric(conf_saved$file_size) && conf_saved$file_size > 0 &&

      # restore session
      length(conf_saved$restore_session) == 1 &&
        isTRUE(conf_saved$restore_session %in% c('always', 'ask', 'never')) &&

      # save session
      length(conf_saved$save_session) == 1 &&
        isTRUE(conf_saved$save_session %in% c('always', 'ask', 'never')) &&

      # fill color
      length(conf_saved$plot_fill_color) == 1 &&
        isTRUE(is_hex_color(conf_saved$plot_fill_color)) &&

      # line color
      length(conf_saved$plot_line_color) == 1 &&
        isTRUE(is_hex_color(conf_saved$plot_line_color)) &&

      # title color
      length(conf_saved$plot_title_color) == 1 &&
        isTRUE(is_hex_color(conf_saved$plot_title_color)) &&

      # plot limit
      length(conf_saved$plot_limit) == 1 &&
        is.numeric(conf_saved$plot_limit)
      ){
      # copy if test is passed
      start_conf$theme <- conf_saved$theme
      start_conf$file_size <- conf_saved$file_size
      start_conf$restore_session <- conf_saved$restore_session
      start_conf$save_session <- conf_saved$save_session
      start_conf$plot_fill_color <- conf_saved$plot_fill_color
      start_conf$plot_line_color <- conf_saved$plot_line_color
      start_conf$plot_title_color <- conf_saved$plot_title_color
      start_conf$plot_limit <- conf_saved$plot_limit
    }
  }
  qs_save(start_conf, conf_path)

  return(start_conf)
}

# generate 2 column table in html ---------------------------------------------
gen_table2 <- function(element1, element2) {
  div(
    tags$table(
      style = 'width: 95%',
      tags$tr(
        tags$td(style = 'padding: 10px; width: 50%;', element1),
        tags$td(style = 'padding: 10px; width: 50%;', element2)
      )
    )
  )
}

# card to insert in output ----------------------------------------------------
report_card <- function(title = 'Spada - Output', annotation = NULL,
                        content = NULL, id = NULL){
  div(
    div(
      style = paste0("border: 2px solid", main_color,
                     "; border-radius: 8px; padding: 16px; box-shadow: 0 4px 4px rgba(0, 0, 0, 0.15);"),
      h2(title, style = paste0("font-size: 1.5rem; margin-bottom: 12px; color:",
                               main_color ,";")),
      p(annotation),
      content
    ),
    br(),
    p(id)
  )
}

# generate element id for outputs ---------------------------------------------
gen_element_id <- function(id = 'id', time_only = FALSE){
  if(time_only){
    gsub('.', '', format(Sys.time(), '%Y%m%d%H%M%OS8'), fixed = T)
  } else {
    paste0(id, '_', gsub('.', '', format(Sys.time(), '%Y%m%d%H%M%OS8'), fixed = T))
  }
}

# allowed operations function -------------------------------------------------
show_allowed_op <- function(){
  showModal(modalDialog(title = "Allowed Operations", HTML(
    paste(allowed_operations, collapse = "<br/>")
  ), easyClose = TRUE))
}

# safe env function -----------------------------------------------------------
safe_env <- function(operations = NULL){

  e <- new.env(parent = emptyenv())

  lapply(operations, function(x) {
    assign(x, get(x), envir = e)
  })

  return(e)
}

# test dataset ----------------------------------------------------------------
test_dataset <- function(n_row = 1e3, n_col = 11){
  test_data <- data.frame(
    integer_var = rep(sample(1:100, n_row, replace = T)),
    numeric_var = rnorm(n_row),
    char_var = rep(sample(letters, n_row, replace = T)),
    char_long_var = rep(paste(letters, collapse = ''), n_row),
    char_colors_var = rep(sample(colors(), n_row, replace = T)),
    date_var = Sys.Date() + rep(sample(-49:50, n_row, replace = T)),
    factor_var = as.factor(rep(sample(paste0('factor_', 1:10), n_row, replace = T))),
    num_nas_var = c(rep(NA, round(n_row/2)), rnorm(n_row - round(n_row/2))),
    int_nas_var = c(rep(NA, round(n_row/2)), sample(1:100, n_row - round(n_row/2), replace = T)),
    logical_var = rep(sample(c(TRUE, FALSE), n_row, replace = T)),
    complex_var = rep(sample(1:100, n_row, replace = T) |> as.complex()),
    all_na = rep(NA, n_row)
  )

  extra_cols <- n_col - ncol(test_data)

  if (extra_cols > 0) {
    for (i in seq_len(extra_cols)) {
      test_data[[paste0("extra_col_", i)]] <- rnorm(n_row)
    }
  }

  test_data
}

# empty plot function ---------------------------------------------------------
empty_plot <- function(msg = 'No plot', c = 2){
  plot(1:10, 1:10, type = 'n', xlab = '', ylab = '')
  text(5, 5, msg, cex = c)
}

# bslib btn task --------------------------------------------------------------
btn_task <- function(ID, LABEL, ICON = NULL, LABEL_BUSY = "Running...", ...){
  bslib::input_task_button(id = ID, label = LABEL, icon = ICON,
                           label_busy = LABEL_BUSY, class = 'btn-task', ...)
}

# messages - shownotification -------------------------------------------------
msg <- function(TEXT, DURATION = 2.3){
  show_toast(
    title = TEXT,
    type = 'info',
    position = 'center',
    timer = DURATION * 1000,
    timerProgressBar = F,
    width = '650px'
  )
}

msg_error <- function(TEXT, DURATION = 2.3){
  show_toast(
    title = TEXT,
    type = 'error',
    position = 'center',
    timer = DURATION * 1000,
    timerProgressBar = F,
    width = '650px'
  )
}

# try convert -----------------------------------------------------------------
try_convert <- function(x, fun){
  tryCatch(fun(x),
           error = function(e) rep(NA, x |> length()),
           warning = function(w) rep(NA, x |> length()))
}

# convert function ------------------------------------------------------------
convert <- function(x, type, date_format = '%Y-%m-%d',
                    date_origin = '1970-01-01'){
  if(x |> is.raw()) x <- as.numeric(x)

  if(type == 'as.numeric'){
    suppressWarnings(as.numeric(x))
  } else if(type == 'as.integer'){
    suppressWarnings(as.integer(x))
  } else if(type == 'as.character'){
    as.character(x)
  } else if(type == 'as.Date'){
    if(x |> inherits('Date')) {
      x
    } else if(is.numeric(x)){
      as.Date(x, origin = date_origin)
    } else if(is.raw(x) || is.complex(x)){
      suppressWarnings(
        as.Date(x |> as.numeric(), origin = date_origin)
      )
    } else as.Date(x, format = date_format)
  } else if(type == 'as.factor'){
    as.factor(x)
  } else if(type == 'as.double'){
    suppressWarnings(as.double(x))
  } else if(type == 'as.complex'){
    if(is.complex(x)){
      x
    } else {
      x1 <- suppressWarnings(as.numeric(x))
      as.complex(x1)
    }
  }
}

# filter rows function --------------------------------------------------------
filter_rows <- function(dt, var, operator, filter_value){

  stopifnot(is.data.table(dt))

  if(operator == '=='){
    dt[var1 == filter_value, , env = list(var1 = var)]
  } else if(operator == '!='){
    dt[var1 != filter_value, , env = list(var1 = var)]
  } else if(operator == '>'){
    dt[var1 > filter_value, , env = list(var1 = var) ]
  } else if(operator == '>='){
    dt[var1 >= filter_value, , env = list(var1 = var) ]
  } else if(operator == '<'){
    dt[var1 < filter_value, , env = list(var1 = var) ]
  } else if(operator == '<='){
    dt[var1 <= filter_value, , env = list(var1 = var) ]
  } else if(operator == 'is_na'){
    dt[is.na(var1), , env = list(var1 = var) ]
  } else if(operator == 'not_na'){
    dt[!is.na(var1), , env = list(var1 = var) ]
  } else if(operator == 'in'){
    dt[var1 %in% filter_value,  , env = list(var1 = var)]
  } else if(operator == 'not_in'){
    dt[!var1 %in% filter_value, , env = list(var1 = var) ]
  } else if(operator == 'between'){
    dt[var1 %between% filter_value, , env = list(var1 = var) ]
  } else if(operator == 'not_between'){
    dt[!(var1 %between% filter_value), , env = list(var1 = var) ]
  } else if(operator == 'outlier'){
    dt[Outlier(var1, value = F, na.rm = T), , env = list(var1 = var)]
  } else if(operator == 'not_outlier'){
    dt[!Outlier(var1, value = F, na.rm = T), , env = list(var1 = var)]
  } else if(operator == 'is_true'){
    dt[var1 == TRUE,  , env = list(var1 = var)]
  } else if(operator == 'is_false'){
    dt[var1 == FALSE,  , env = list(var1 = var)]
  }
}

# filter rows 2 vars ----------------------------------------------------------
filter_rows_2vars <- function(dt, var1, var2, operator){

  stopifnot(is.data.table(dt))

  v1 <- dt[[var1]]
  v2 <- dt[[var2]]

  # Convert to char if factor
  if (is.factor(v1) && is.factor(v2)) {
    v1 <- as.character(v1)
    v2 <- as.character(v2)
  }

  if(operator == '=='){
    dt[v1 == v2, ]
  } else if(operator == '!='){
    dt[v1 != v2, ]
  } else if(operator == '>'){
    dt[v1 > v2, ]
  } else if(operator == '>='){
    dt[v1 >= v2, ]
  } else if(operator == '<'){
    dt[v1 < v2, ]
  } else if(operator == '<='){
    dt[v1 <= v2, ]
  } else {
    stop('Invalid Operator')
  }
}

# tip place top by default ----------------------------------------------------
ttip <- function(TRIGGER, ..., ID = NULL, PLACE = 'top'){
  tooltip(trigger = TRIGGER, ... = ..., id = ID, placement = PLACE)
}

# get function help -----------------------------------------------------------
get_help_file <- function(pak, fun){
  paste(
    utils::capture.output(
      tools::Rd2HTML(tools::Rd_db(pak)[[paste0(fun, '.Rd')]])
    ),
    collapse = '\n'
  )
}

# format decimals -------------------------------------------------------------
f_dec <- function(x, dig = 0){
  if(is.numeric(x) && !is.na(x) |> all()){
    format(round(x, dig), nsmall = dig, scientific = F)
  } else {
    NA
  }
}

# make var names --------------------------------------------------------------
make_var_names <- function(df){
  stopifnot(df |> is.data.frame())
  names(df) <- names(df) |> make.names(unique = T)
  return(df)
}

# test all equal --------------------------------------------------------------
test_all_equal <- function(x){
  all(x == x[1])
}

# obj type --------------------------------------------------------------------
obj_type <- function(x){
  if(x |> is.numeric()) 'numeric'
  else if (x |> is_date()) 'date'
  else if (x |> is.factor()) 'factor'
  else if (x |> is.character()) 'char'
  else if (x |> is.logical()) 'logical'
  else if (x |> is.complex()) 'complex'
  else 'other'
}

# stati_card ------------------------------------------------------------------
stati_card <- function(VALUE, SUBTITLE, ICON = NULL, LEFT = T,
                       COLOR = '#FFFFFF', BACKGROUND = main_color,
                       ANIMATE = T, DURATION = 30, ID = NULL){
  statiCard(value = VALUE,
            subtitle = SUBTITLE,
            icon = ICON,
            left = LEFT,
            color = COLOR,
            background = BACKGROUND,
            animate = ANIMATE,
            duration = DURATION,
            id = ID)
}

# make valid cols -------------------------------------------------------------
make_valid_cols <- function(x){
  if(is.raw(x) || is.complex(x)){
    as.character(x)
  } else {
    x
  }
}

# is data frame ---------------------------------------------------------------
is_spada_df <- function(df){
  is.data.frame(df) && all(sapply(df, is.atomic))
}

# show startup function -------------------------------------------------------
show_startup_screen <- function() {
  waiterShowOnLoad(
    html = tagList(div(
      class = 'screen-container',
      div('Spada', class = 'screen-title'),
      div('a Shiny Package for Data Analysis', class = 'screen-subtitle2'),
      waiter_spinner
      ),
      waiter_right_foot
    )
  )
}

# exit screen function --------------------------------------------------------
show_exit_screen <- function(save = TRUE) {
  waiter_show(
    html = {
      if(isTRUE(save)){
        tagList(
          div(
            class = "screen-container",
            div("Spada is saving your work", class = "screen-subtitle"),
            div("Please do not close this window", class = "screen-subtitle2"),
            waiter_spinner
          ),
          waiter_right_foot
        )
      } else {
        html = tagList(
          div(
            class = "screen-container",
            div("Closing Spada", class = "screen-subtitle"),
            waiter_spinner
          ),
          waiter_right_foot
        )
      }
    }
  )
}

# exit spada with saving session ----------------------------------------------
exit_with_save <- function(session){

  show_exit_screen()

  t0 <- Sys.time()

  check_dir(session$userData$conf$data_dir)
  qs_save(session$userData$out$elements,
          paste0(session$userData$conf$data_dir, '/output.qs2'))

  check_dir(session$userData$conf$data_dir)
  qs_save(session$userData$dt$dt,
          paste0(session$userData$conf$data_dir, '/data.qs2'))

  check_dir(session$userData$conf$conf_dir)
  qs_save(reactiveValuesToList(session$userData$conf),
          paste0(session$userData$conf$conf_dir, '/conf.qs2'))

  t_diff <- Sys.time() - t0
  if(t_diff < 5) Sys.sleep(5 - t_diff)
  session$sendCustomMessage(type = 'closeWindow', message = 'message')
  stopApp()
}

# exit spada without saving session -------------------------------------------
exit_without_save <- function(session){

  show_exit_screen(F)

  check_dir(session$userData$conf$conf_dir)
  qs_save(reactiveValuesToList(session$userData$conf),
          paste0(session$userData$conf$conf_dir, '/conf.qs2'))

  Sys.sleep(3)
  session$sendCustomMessage(type = 'closeWindow', message = 'message')
  stopApp()
}

# make names append lists -------------------------------------------------
make_names_append_list <- function(new_list, actual_names,
                                   suffix = '_previous', index = 1) {

    names(new_list) <- make.names(names(new_list), unique = TRUE)

    while (any(grepl(paste0("^.*", suffix, "$"), actual_names))) {
      suffix <- paste0(suffix, "_", i)
      index <- index + 1
    }

    conflict_names <- names(new_list) %in% actual_names
    names(new_list)[conflict_names] <- paste0(names(new_list)[conflict_names], suffix)

    return(new_list)
}

# plot z test -----------------------------------------------------------------
plot_z_test <- function(confidence = 0.95, test_type = 'two.sided',
                        z_value = qnorm(confidence),
                        color_fill = 'brown3', color_line = 'steelblue') {
  # Define the standardized x-axis (Z-scores)
  x <- seq(-4, 4, length.out = 1000)  # Standardized Z-values
  y <- dnorm(x, mean = 0, sd = 1)  # Standard normal distribution

  alpha <- 1 - confidence

  # Determine critical values in standardized scale
  if (test_type == 'two.sided') {
    z_critical <- qnorm(1 - alpha / 2)
    critical_left <- -z_critical
    critical_right <- z_critical
  } else if (test_type == 'greater') {
    critical_right <- qnorm(1 - alpha)
  } else if (test_type == 'less') {
    critical_left <- qnorm(alpha)
  } else {
    stop("Invalid 'test_type'. Use 'two.sided', 'greater', or 'less'.")
  }

  # Create the plot
  plot(x, y, type = "l", lwd = 2, col = color_line,
       xlab = 'Standardized Values (Z)', ylab = 'Density',
       main = paste('Test Type:', test_type, ' - Confidence:',
                    confidence * 100, '% | Z value:', z_value |> f_num(dig = 3)))

  # Highlight the critical regions
  if (test_type == 'two.sided') {
    polygon(c(x[x <= critical_left], critical_left),
            c(y[x <= critical_left], 0), col = color_fill, border = NA)
    polygon(c(x[x >= critical_right], critical_right),
            c(y[x >= critical_right], 0), col = color_fill, border = NA)

  } else if (test_type == 'less') {
    polygon(c(x[x <= critical_left], critical_left),
            c(y[x <= critical_left], 0), col = color_fill, border = NA)

  } else if (test_type == 'greater') {
    polygon(c(x[x >= critical_right], critical_right),
            c(y[x >= critical_right], 0), col = color_fill, border = NA)
  }

  if(abs(z_value) <= 4){
    abline(v = z_value, col = 'black', lwd = 2, lty = 2)
  }

}

# test if is hex color --------------------------------------------------------
is_hex_color <- function(x) {
  grepl("^#[A-Fa-f0-9]{6}$", x)
}

# test output format-----------------------------------------------------------
test_output_format <- function(output){

  # if list of len 0  is OK
  (is.list(output) && length(output) == 0) ||

  # if len > 0 must pass
  (is.list(output) && length(output) > 0 &&

     # all inside elements must be lists
     all(sapply(output, class) == 'list') &&

     # all names of inside lists must match
     all(
       sapply(output, \(x) {
         all(names(x) %in% c('id', 'title', 'card'))
       })
     ) &&
    # test class of each element in the inside lists
      (
        all(sapply(output, \(x){ x$id |> class() == 'character'})) &&
          all(sapply(output, \(x){ x$title |> class() == 'character'})) &&
          all(sapply(output, \(x){ x$card |> class() == 'shiny.tag'}))
      )
  )
}

# test data format-------------------------------------------------------------
test_data_format <- function(data){
  is.list(data) &&
     length(data) > 0 &&
     (sapply(data, is.data.frame) |> all()) &&
     (all(sapply(data, nrow) > 0))
}

# check existence of directory ------------------------------------------------
check_dir <- function(dir){
  if(!dir.exists(dir)) dir.create(dir, recursive = T)
}

# linear model df output ------------------------------------------------------
linear_model_df_output <- function(model_summary){
  stopifnot(model_summary |> class() == 'summary.lm')

  table_summary <- as.data.frame(model_summary$coefficients)

  table_summary$Variable <- rownames(table_summary)
  rownames(table_summary) <- NULL

  table_summary <- table_summary[, c("Variable", "Estimate", "Std. Error",
                                     "t value", "Pr(>|t|)")]

  table_summary <- table_summary |> as.data.table()
  table_summary[, `Sig Levels` := fcase(
    `Pr(>|t|)` < 0.001, '***',
    `Pr(>|t|)` < 0.01, '**',
    `Pr(>|t|)` < 0.05, '*',
    `Pr(>|t|)` < 0.1, '.',
    default = ''
  )]
}

# linear model gt metrics -----------------------------------------------------
linear_model_df_metrics <- function(model_summary){
  stopifnot(model_summary |> class() == 'summary.lm')

  f <- model_summary$fstatistic
  f_p_value <- pf(f['value'], f['numdf'], f['dendf'], lower.tail = FALSE)
  data.frame(
    Metric = c('Residual Std Error', 'R-squared', 'Adjusted R-squared',
               'F-statistic', 'F p-value'),
    Value = c(
      model_summary$sigma,
      model_summary$r.squared,
      model_summary$adj.r.squared,
      model_summary$fstatistic[1],
      f_p_value
    )
  )
}

# get active dt ---------------------------------------------------------------
get_act_dt <- function(session){
  session$userData$dt$dt[[ session$userData$dt$act_name ]]
}

# update active dt ------------------------------------------------------------
update_act_dt <- function(session, new_dt, data_changed = TRUE) {

  stopifnot(new_dt |> is.data.frame())

  new_dt <- lapply(new_dt, make_valid_cols) |> as.data.table()

  session$userData$dt$dt[[ session$userData$dt$act_name ]] <- new_dt

  if(data_changed) {
    session$userData$data_changed(session$userData$data_changed() + 1)
  }
}

# append dt -------------------------------------------------------------------
append_dt <- function(session, new_dt, new_dt_name) {

  stopifnot(new_dt |> is.data.frame())
  stopifnot(new_dt_name |> is_valid_name() &&
              new_dt_name %notin% names(session$userData$dt$dt))

  new_dt <- lapply(new_dt, make_valid_cols) |> as.data.table()
  new_list <- list(new_dt)
  names(new_list) <- new_dt_name

  session$userData$dt$dt <- c(session$userData$dt$dt, new_list)
}

# append meta data ------------------------------------------------------------
append_meta <- function(session, new_meta, new_dt_name){
  stopifnot(new_meta |> is.data.frame())

  meta_list <- list(new_meta)
  names(meta_list) <- new_dt_name

  session$userData$dt$meta <- c(
    session$userData$dt$meta,
    meta_list
  )
}

# descriptive stats -----------------------------------------------------------
desc_stats <- function(df = NULL,
                       fmt_digits = 9,
                       central_tendency = c('mean', 'gmean', 'hmean', 'median', 'mode'),
                       dispersion = c('min', 'max', 'IQR', 'range', 'var', 'sd'),
                       shape = c('skew', 'kurt')) {

  desc_stats <- list()

  # central tendency
  if('mean' %in% central_tendency){
    desc_stats$Mean <- sapply(
      df,
      \(x) {if(x |> is.numeric()) mean(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
  }

  if('gmean' %in% central_tendency){
    desc_stats$Gmean <- sapply(
      df,
      \(x) {if(x |> is.numeric()) suppressWarnings(Gmean(x, na.rm = T)) |> f_num(dig = fmt_digits) else NA })
  }

  if('hmean' %in% central_tendency){
    desc_stats$Hmean <- sapply(
      df,
      \(x) {if(x |> is.numeric()) Hmean(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
  }

  if('median' %in% central_tendency){
    desc_stats$Median <- sapply(
      df,
      \(x) {if(x |> is.numeric()) median(x, na.rm = T) |> f_num(dig = fmt_digits)  else NA })
  }

  if('mode' %in% central_tendency){
    desc_stats$Mode <- sapply(
      df,
      \(x) {if(x |> is.numeric() ||
               x |> is.character() ||
               x |> is.factor()){
        x_mode <- Mode(x, na.rm = T) |> f_num(dig = fmt_digits)
        if(is.na(x_mode) |> all()) NA else paste(x_mode, collapse = ' | ')
      } else { NA }
      })
  }

  # dispersion
  if('min' %in% dispersion){
    desc_stats$Min <- sapply(
      df,
      \(x) {if(x |> is.numeric()) mina(x) |> f_num(dig = fmt_digits) else NA })
  }

  if('max' %in% dispersion){
    desc_stats$Max <- sapply(
      df,
      \(x) {if(x |> is.numeric()) mana(x) |> f_num(dig = fmt_digits) else NA })
  }

  if('IQR' %in% dispersion){
    desc_stats$IQR <- sapply(
      df,
      \(x) {if(x |> is.numeric()) IQR(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
  }

  if('range' %in% dispersion){
    desc_stats$Range <- sapply(
      df,
      \(x) {
        if(x |> is.numeric()){
          paste('[', range(x) |> f_num(dig = fmt_digits) , ']', collapse = '--->')
        } else { NA }
      }
    )
  }

  if('var' %in% dispersion){
    desc_stats$Variance <- sapply(
      df,
      \(x) {if(x |> is.numeric()) var(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
  }

  if('sd' %in% dispersion){
    desc_stats[['Standard Deviation']] <- sapply(
      df,
      \(x) {if(x |> is.numeric()) sd(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
  }

  if('skew' %in% shape){
    desc_stats[['Skewness']] <- sapply(
      df,
      \(x) {if(x |> is.numeric()) Skew(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
  }

  if('kurt' %in% shape){
    desc_stats[['Kurtosis']] <- sapply(
      df,
      \(x) {if(x |> is.numeric()) Kurt(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
  }

  desc_stats
}

# plots -----------------------------------------------------------------------
spada_plot <- function(
    type = 'hist',
    df = NULL,
    xvar = NULL,
    yvar = NULL,
    zvar = NULL,
    xlab = '',
    ylab = '',
    fill_color = '#0099F8',
    line_color = '#000000',
    title_color = '#02517d',
    title = NULL,
    bins = 25,
    vertical_line = NULL,
    point_shape = '.',
    line_type = 1,
    mean_value = NULL,
    sd_value = NULL,
    sample_limit = 1e5
  ){

  if(nrow(df) > sample_limit){
    df <- df[sample.int(nrow(df), sample_limit), , drop = FALSE]
  }

  stopifnot(is.data.frame(df))

  if(type == 'hist'){

    ggplot(data = df, aes(x = .data[[xvar]])) +
      geom_histogram(
        bins = bins,
        fill = fill_color,
        color = '#000000'
      ) +
      geom_vline(xintercept = vertical_line, color = line_color, linetype = line_type) +
      labs(x = xlab, y = ylab, title = title) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(color = title_color, size = 16, face = 'bold')
      )
  } else if (type == 'hist_density'){

    ggplot(data = df, aes(x = .data[[xvar]])) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = bins,
                     fill = fill_color,
                     color = 'black') +
      stat_function(fun = dnorm,
                    args = list(mean = mean_value,
                                sd = sd_value),
                    color = line_color,
                    linewidth = 1) +
      labs(x = xlab, y = ylab, title = title) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(color = title_color, size = 16, face = 'bold')
      )

  } else if (type == 'boxplot'){

    ggplot(data = df, aes(x = .data[[xvar]])) +
      stat_boxplot(geom = 'errorbar', width = 0.3) +
      geom_boxplot(fill = fill_color) +
      ylim(-1.2, 1.2) +
      geom_vline(xintercept = vertical_line, color = line_color, linetype = line_type) +
      labs(x = '', y = '') +
      theme_classic() +
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.line.y  = element_blank(),
        panel.border = element_rect(color = '#000000', fill = NA),
        axis.text.x = element_text(size = 14)
      )
  } else if(type == 'dots'){

    ggplot(data = df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
      geom_point(shape = point_shape, color = fill_color) +
      geom_hline(yintercept = vertical_line, color = line_color, linetype = line_type) +
      labs(x = xlab, y = ylab) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)
      )
  } else if (type == 'barplot'){

    ggplot(data = df, aes(x = factor(.data[[xvar]]))) +
      geom_bar(fill = fill_color) +
      labs(x = xlab, y = ylab) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 16)
      )

  } else if (type == 'boxplot_group'){

    ggplot(data = df, aes(x = .data[[xvar]], y = .data[[yvar]], fill = .data[[xvar]])) +
      stat_boxplot(geom = 'errorbar', width = 0.3) +
      geom_boxplot(orientation = 'x') +
      geom_hline(yintercept = vertical_line,
                 color = plot_line_color) +
      coord_flip() +
      labs(x = xlab, y = ylab) +
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.ticks.y = element_blank(),
        axis.line.y  = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
      )
  } else if (type == 'scatter'){

    ggplot(data = df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
      geom_point(color = fill_color, shape = point_shape) +
      labs(title = title,
           x = xlab, y = ylab) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(color = title_color, size = 16, face = 'bold')
      )
  } else if (type == 'qq_plot'){
    ggplot(data = df, aes(sample = .data[[xvar]])) +
      stat_qq(color = fill_color) +
      stat_qq_line(color = line_color) +
      labs(title = title, x = xlab, y = ylab) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(color = title_color, size = 16, face = 'bold')
      )
  }
}

# status row for restore session ----------------------------------------------
status_row <- function(icon_name, color, text) {
  div(
    style = paste0(
      'display:flex; align-items:center; gap:12px;
       padding:12px 16px;
       border-radius:2px;
       background-color:', color, '15;'
    ),
    icon(icon_name, style = paste0('font-size:20px; color:', color)),
    tags$span(text, style = 'font-size:20px; font-weight:400;')
  )
}

# display restore status ------------------------------------------------------

display_restore_status <- function(session_restore_status){
  list_check_restore <- div(
    style = 'display:flex; flex-direction:column; gap:12px;',

    switch(
      substr(session_restore_status, 1, 1),
      '1' = status_row('check', '#2e7d32', 'Data restored successfully'),
      '2' = status_row('times', '#c62828', 'Data not found'),
      '3' = status_row('circle-question', '#ed6c02', 'Data in invalid format')
    ),

    switch(
      substr(session_restore_status, 3, 3),
      '1' = status_row('check', '#2e7d32', 'Output restored successfully'),
      '2' = status_row('times', '#c62828', 'Output not found'),
      '3' = status_row('circle-question', '#ed6c02', 'Output in invalid format')
    )
  )

  # show modal
  showModal(modalDialog(
    title = div(
      h1(bs_icon('database-up', size = '55px', style = 'margin-right: 8px; color:#02517d'),
        'Session Restore Status'
      )
    ),
    div(style = 'padding:12px; border-radius:0px;', list_check_restore),
    size = 'l',
    easyClose = F,
    footer = div(
      style = 'text-align:right;',
      actionButton('btn_dismiss_restore_sesison', 'OK', class = 'btn-task')
    )
  ))

}

# summarise dataset -----------------------------------------------------------
summarise_dt <- function(dt, fun, vars){

  stopifnot(is.data.table(dt))

  if(fun == 'distinct'){
    dt[, .SD, .SDcols = vars] |> unique()
  } else if(fun == 'count'){
    dt[, .N, by = vars]
  }
}
