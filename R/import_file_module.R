
# ui --------------------------------------------------------------------------
import_file_ui <- function(id) {
  file_extensions <- c('csv', 'RDS', 'sav')
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
            card_header('Sav Parameters', class = 'mini-header'),
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

    data <- reactiveValues(data = NULL)

    observe({
      if (is.null(input$file)) {
        msg_error('Insert a file')
        return()
      }

      if (!is_valid_name(input$dataset_name) ||
          input$dataset_name %in% session$userData$dt_names()) {
        msg_error('Name invalid or already in use')
        return()
      }

      # import file
      ext <- file_ext(input$file$datapath) |> tolower()

      if (ext == 'csv' && input$radio_file_ext == 'csv'){
        n <- if (input$csv_lines == 'all') Inf else as.integer(input$csv_n_lines)
        if (n < 1) {
          msg_error('Number of lines must be at least 1')
          return()
        }
        data$data <- tryCatch({
          fread(input$file$datapath,
                sep = input$radio_separator,
                dec = input$radio_decimal,
                header = input$x_csv_header,
                nrows = n,
                check.names = TRUE)
          },
          warning = \(w) msg_error('Check parameters'),
          error = \(e) msg_error('Check parameters')
        )
      } else if (ext == 'rds' && input$radio_file_ext == 'RDS'){
        data$data <- readRDS(input$file$datapath)
        if (!is_spada_df(data$data)) {
          msg_error('Object must be data.frame')
          return()
        }
        data$data <- as.data.table(data$data)

      } else if (ext == 'sav' && input$radio_file_ext == 'sav'){
        n <- if (input$sav_lines == 'all') Inf else as.integer(input$sav_n_lines)
        if (n < 1) {
          msg_error('Number of lines must be at least 1')
          return()
        }
        data$data <- read_sav(input$file$datapath, n_max = n,
                                  .name_repair = make.names) |>
          as.data.table()
        data$data[, names(.SD) := lapply(.SD, as_factor), .SDcols = is.labelled]

      } else {
        msg_error(paste('Insert a', input$radio_file_ext, 'file'))
        return()
      }

      # update dt ----------------------------
      data$data <- data$data |> make_var_names()
      append_dt(session, data$data, input$dataset_name)

      # update metadata ----------------------
      new_meta <- list()

      new_meta[[input$dataset_name]] <- data$data |> df_info()

      session$userData$dt$meta <- c(
        session$userData$dt$meta,
        new_meta
      )

      msg('File imported successfully')

    }) |> bindEvent(input$btn_import)

    # preview csv file --------------------------------------------------------
    observe({
      req(input$file)

        ext <- file_ext(input$file$datapath) |> tolower()

        if (ext == 'csv' && input$radio_file_ext == 'csv') {
          preview_data <- readLines(input$file$datapath, n = 8)

          showModal(modalDialog(
            size = 'xl',
            title = 'Preview Raw File',
            HTML(paste(preview_data, collapse = '<br>'))
          ))

        } else {
          msg_error(paste('Insert a', input$radio_file_ext, 'file'))
        }
      }) |> bindEvent(input$btn_preview_raw)

  })
}
