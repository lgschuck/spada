
# ui --------------------------------------------------------------------------
import_file_ui <- function(id) {
  file_extensions <- c('csv', 'rds', 'sav')
  ns <- NS(id)
    card(
      card_body(
        fluidRow(
          column(3, textInput(ns('dataset_name'), 'Dataset name', 'dataset')),
          column(3, radioButtons(ns('radio_file_ext'), 'File format',
                                        file_extensions, inline = T)),
          column(3, fileInput(ns('file'), 'Choose a File')),
        ),
        conditionalPanel(
          condition = "input.radio_file_ext == 'csv'", ns = ns,
          card(
            card_header('Csv Parameters', class = 'mini-header'),
            card_body(
              layout_column_wrap(
                checkboxInput(ns('x_csv_header'), 'Header', T),
                radioButtons(ns('radio_separator'), 'Separator',
                             c('Semicolon' = ';', 'Comma' = ','), inline = T),
                radioButtons(ns('radio_decimal'), 'Decimal Mark',
                             c('Dot' = '.', 'Comma' = ','), inline = T),
                radioButtons(ns('csv_lines'), 'Lines to read',
                             choices = c('All' = 'all', 'Specific (N)' = 'some'), inline = TRUE),
                conditionalPanel(
                  condition = "input.csv_lines == 'some'", ns = ns,
                  numericInput(ns('csv_n_lines'), NULL, width = '150px',
                               value = 100, min = 1, step = 100) |>
                    ttip('You may use Scientific Notation, e.g. 1e6')
                ),
                btn_task(ns('btn_preview_raw'), 'Preview raw file', icon('magnifying-glass')),
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.radio_file_ext == 'sav'", ns = ns,
          card(
            card_header('SAV Parameters', class = 'mini-header'),
            card_body(
              layout_columns(
                col_widths = c(2, 2),
                radioButtons(ns('sav_lines'), 'Lines to read',
                             choices = c('All' = 'all', 'Specific (N)' = 'some'), inline = TRUE),
                conditionalPanel(
                  condition = "input.sav_lines == 'some'", ns = ns,
                  numericInput(ns('sav_n_lines'), NULL, width = '150px',
                               value = 100, min = 1, step = 100) |>
                    ttip('You may use Scientific Notation, e.g. 1e6')
                )
              )
            )
          )
        )
      ),
      card_footer(
        btn_task(ns('btn_import'), 'Import file', icon('upload')),
      )
    )
}

# server ----------------------------------------------------------------------
import_file_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    data <- reactiveValues(
      data = NULL,
      data_name = NULL,
      temp = NULL,
      ext = NULL,
      file = NULL
      )

    # import file -------------------------------------------------------------
    observe({
      if(is.null(input$file)){
        msg_error('Insert a file')
      } else if(!is_valid_name(input$dataset_name) ||
                input$dataset_name %in% session$userData$dt_names()){
        msg_error('Name invalid or already in use')
      } else {
        data$file <- input$file
        data$ext <- tools::file_ext(data$file$datapath)

        if (tolower(data$ext) == 'csv' && input$radio_file_ext == 'csv') {

          if (input$csv_lines == 'all') {
            n <- Inf
          } else if(!isTruthy(input$csv_n_lines) || input$csv_n_lines < 1){
            msg_error('Number of lines must be at least 1')
            return()
          } else {
            n <- as.integer(input$csv_n_lines)
          }

          tryCatch(
            { data$data <- fread(
              file = data$file$datapath,
              sep = input$radio_separator,
              dec = input$radio_decimal,
              check.names = T,
              nrows = n,
              skip = 0,
              header = input$x_csv_header)

              data$data_name <- input$dataset_name

              msg('File imported')
            },
            warning = \(w) msg_error('Check parameters'),
            error = \(e) msg_error('Check parameters')
          )

        } else if (tolower(data$ext) == 'rds' && input$radio_file_ext == 'rds') {

          data$temp <- readRDS(data$file$datapath)

          if(data$temp |> is.data.frame()){
            data$data <- data$temp |> as.data.table()
            data$data_name <- input$dataset_name

            msg('File imported')
          } else {
            msg_error('Object must be data.frame')
          }
        } else if(tolower(data$ext) == 'sav' && input$radio_file_ext == 'sav'){
          if (input$sav_lines == 'all') {
            n <- Inf
          } else if(!isTruthy(input$sav_n_lines) || input$sav_n_lines < 1){
            msg_error('Number of lines must be at least 1')
            return()
          } else {
            n <- as.integer(input$sav_n_lines)
          }

          data_temp <- read_sav(data$file$datapath, n_max = n ,
                                .name_repair = make.names) |>
            as.data.table()

          # convert labelled variables (avoid gt package error)
          data_temp[ , names(.SD) := lapply(.SD, as_factor), .SDcols = is.labelled]

          data$data <- data_temp
          data$data_name <- input$dataset_name

          msg('File imported')

        } else {
          msg_error(paste('Insert a', input$radio_file_ext, 'file'))
        }
      }

    }) |> bindEvent(input$btn_import)

    # preview csv file --------------------------------------------------------
    observe({
      req(input$file)

        data$file <- input$file
        data$ext <- tools::file_ext(data$file$datapath)

        if (tolower(data$ext) == 'csv' && input$radio_file_ext == 'csv') {
          preview_data <- readLines(data$file$datapath, n = 8)

          showModal(modalDialog(
            size = 'xl',
            title = 'Preview Raw File',
            HTML(paste(preview_data, collapse = '<br>'))
          ))

        } else {
          msg_error(paste('Insert a', input$radio_file_ext, 'file'))
        }
      }) |> bindEvent(input$btn_preview_raw)

    # update dt ---------------------------------------------------------------
    observe({
      req(data$data)
      req(data$data_name)

      data$data <- lapply(data$data, make_valid_cols) |> as.data.table()

      session$userData$dt$dt[[data$data_name]] <- data$data
    })

  })
}
