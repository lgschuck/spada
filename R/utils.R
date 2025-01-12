
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

  "))
)

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
msg <- function(TEXT, DURATION = 2){
  showNotification(ui = TEXT, duration = DURATION, type = 'message')
}

msg_error <- function(TEXT, DURATION = 2){
  showNotification(ui = TEXT, duration = DURATION, type = 'error')
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
    df[get(var) == filter_value, ]
  } else if(operator == '!='){
    df[get(var) != filter_value, ]
  } else if(operator == '>'){
    df[get(var) > filter_value, ]
  } else if(operator == '>='){
    df[get(var) >= filter_value, ]
  } else if(operator == '<'){
    df[get(var) < filter_value, ]
  } else if(operator == '<='){
    df[get(var) <= filter_value, ]
  } else if(operator == 'is_na'){
    df[is.na(get(var)), ]
  } else if(operator == 'not_na'){
    df[!is.na(get(var)), ]
  } else if(operator == 'in'){
    df[get(var) %in% filter_value, ]
  } else if(operator == 'not_in'){
    df[!get(var) %in% filter_value, ]
  } else if(operator == 'between'){
    df[get(var) %between% filter_value, ]
  } else if(operator == 'not_between'){
    df[!(get(var) %between% filter_value), ]
  }
}

# palettes --------------------------------------------------------------------

gray_palette <- c('#ffffff', '#585858', '#232323')
blue_palette <- c('#ffffff', '#096691', '#134359')
yl_palette <- c('#ffffff', '#ffc107', '#f7a305')
dg_palette <- c('#ffffff','#1c6561', '#284e4c')
lg_palette <- c('#ffffff', '#0cb0a8', '#09918b')
pk_palette <- c('#ffffff', '#bf007f', '#8f0360')
red_palette <- c('#ffffff', '#b60020', '#750217')
