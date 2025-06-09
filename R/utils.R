

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
                        content = NULL){
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
    p(Sys.time())
  )
}

# generate element id for outputs ---------------------------------------------
gen_element_id <- function(){
  paste0('element_', format(Sys.time(), '%Y%m%d%H%M%OS8'))
}

# math functions --------------------------------------------------------------

math_funs <- c(
  c('Mean' = 'mean',
    'Geometric Mean' = 'Gmean',
    'Harmonic Mean' = 'Hmean',
    # 'Mode' = 'Mode',
    'Standard Deviation' = 'sd',
    'Variance' = 'var',
    'Min' = 'mina',
    'Max' = 'mana',
    'First' = 'fina',
    'Last' = 'lana',
    'Lag' = 'shift',
    # 'Range' = 'range',
    'IQR' = 'IQR',
    'Skewness' = 'Skew',
    'Kurtosis' = 'Kurt',
    'Sum' = 'suna',
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
  'Is Date' = 'is_date'
)

# factor functions ------------------------------------------------------------

factor_funs <- c(
  'Number of Levels' = 'nlevels',
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
  '%notin%', 'between', '%between%', 'fifelse',

  # DescTools package
  'Outlier',

  # dplyr package
  'if_else',

  #stats package
  'quantile',

  #spada package
  'fina', 'lana', 'mana', 'mina', 'suna'
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

# allowed operations function -------------------------------------------------
show_allowed_op <- function(){
  showModal(modalDialog(title = "Allowed Operations", HTML(
    paste(allowed_operations, collapse = "<br/>")
  ), easyClose = TRUE))
}

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

# safe env function -----------------------------------------------------------

safe_env <- function(operations = NULL){

  e <- new.env(parent = emptyenv())

  lapply(operations, function(x) {
    assign(x, get(x), envir = e)
  })

  return(e)
}

# test dataset ----------------------------------------------------------------

test_dataset <- function(n = 1e3){
  data.frame(
    integer_var = rep(sample(1:100, n, replace = T)),
    numeric_var = rnorm(n),
    char_var = rep(sample(letters, n, replace = T)),
    char_long_var = rep(paste(letters, collapse = ''), n),
    char_colors_var = rep(sample(colors(), n, replace = T)),
    date_var = Sys.Date() + rep(sample(-49:50, n, replace = T)),
    factor_var = as.factor(rep(sample(paste0('factor_', 1:10), n, replace = T))),
    num_nas_var = c(rep(NA, round(n/2)), rnorm(n - round(n/2))),
    int_nas_var = c(rep(NA, round(n/2)), sample(1:100, n - round(n/2), replace = T)),
    logical_var = rep(sample(c(TRUE, FALSE), n, replace = T)),
    complex_var = rep(sample(1:100, n, replace = T) |> as.complex())
  )

}

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

# close browser tab -----------------------------------------------------------
js_exit <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"

tag_js_exit <- tags$head(tags$script(HTML(js_exit)))

# empty plot function ---------------------------------------------------------
empty_plot <- function(msg = 'No plot', c = 2){
  plot(1:10, 1:10, type = 'n', xlab = '', ylab = '')
  text(5, 5, msg, cex = c)
}

# bslib btn task --------------------------------------------------------------
btn_task <- function(ID, LABEL, ICON = NULL, ...){
  bslib::input_task_button(id = ID, label = LABEL, icon = ICON,
                           class = 'btn-task', ...)
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

# format conversion -----------------------------------------------------------
convert <- function(x, type, date_format = '%Y-%m-%d',
                    date_origin = '1970-01-01'){
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

# filter function -------------------------------------------------------------
filter_rows <- function(df, var, operator, filter_value){
  if(operator == '=='){
    df[var1 == filter_value, , env = list(var1 = var)]
  } else if(operator == '!='){
    df[var1 != filter_value, , env = list(var1 = var)]
  } else if(operator == '>'){
    df[var1 > filter_value, , env = list(var1 = var) ]
  } else if(operator == '>='){
    df[var1 >= filter_value, , env = list(var1 = var) ]
  } else if(operator == '<'){
    df[var1 < filter_value, , env = list(var1 = var) ]
  } else if(operator == '<='){
    df[var1 <= filter_value, , env = list(var1 = var) ]
  } else if(operator == 'is_na'){
    df[is.na(var1), , env = list(var1 = var) ]
  } else if(operator == 'not_na'){
    df[!is.na(var1), , env = list(var1 = var) ]
  } else if(operator == 'in'){
    df[var1 %in% filter_value,  , env = list(var1 = var)]
  } else if(operator == 'not_in'){
    df[!var1 %in% filter_value, , env = list(var1 = var) ]
  } else if(operator == 'between'){
    df[var1 %between% filter_value, , env = list(var1 = var) ]
  } else if(operator == 'not_between'){
    df[!(var1 %between% filter_value), , env = list(var1 = var) ]
  } else if(operator == 'outlier'){
    df[Outlier(var1, value = F, na.rm = T), , env = list(var1 = var)]
  } else if(operator == 'not_outlier'){
    df[!Outlier(var1, value = F, na.rm = T), , env = list(var1 = var)]
  } else if(operator == 'is_true'){
    df[var1 == TRUE,  , env = list(var1 = var)]
  } else if(operator == 'is_false'){
    df[var1 == FALSE,  , env = list(var1 = var)]
  }
}

# filter rows 2 vars ----------------------------------------------------------
filter_rows_2vars <- function(df, var1, var2, operator){
  v1 <- df[[var1]]
  v2 <- df[[var2]]

  # Convert to char if factor
  if (is.factor(v1) && is.factor(v2)) {
    v1 <- as.character(v1)
    v2 <- as.character(v2)
  }

  if(operator == '=='){
    df[v1 == v2, ]
  } else if(operator == '!='){
    df[v1 != v2, ]
  } else if(operator == '>'){
    df[v1 > v2, ]
  } else if(operator == '>='){
    df[v1 >= v2, ]
  } else if(operator == '<'){
    df[v1 < v2, ]
  } else if(operator == '<='){
    df[v1 <= v2, ]
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
    names(df) <- names(df) |> make.names(unique = T)
    return(df)
}

# test all equal --------------------------------------------------------------
test_all_equal <- function(x){
  all(x == x[1])
}

# col type --------------------------------------------------------------------
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

  if(is.raw(x)){
    as.character(x)
  } else {
    x
  }
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
