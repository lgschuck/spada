
# ui --------------------------------------------------------------------------
import_file_ui <- function(id) {
  file_extensions <- c('csv', 'RDS')
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
          condition = sprintf("input['%s'] == 'csv'", ns('radio_file_ext')),
          card(
            card_header('Csv Parameters', class = 'mini-header'),
            card_body(
              layout_column_wrap(
                checkboxInput(ns('x_csv_header'), 'Header', T),
                radioButtons(ns('radio_separator'), 'Separator',
                             c('Semicolon' = ';', 'Comma' = ','), inline = T),
                radioButtons(ns('radio_decimal'), 'Decimal Mark',
                             c('Dot' = '.', 'Comma' = ','), inline = T),
                btn_task(ns('btn_preview_raw'), 'Preview raw file', icon('magnifying-glass')),
              ),
            )
          )
        ),
      ),
      card_footer(
        btn_task(ns('btn_import'), 'Import file', icon('upload')),
      )
    )
}

# server ----------------------------------------------------------------------
import_file_server <- function(id, current_names) {
  moduleServer(id, function(input, output, session) {

    data <- reactiveValues(data = NULL, data_name = NULL)

    observe({
      if(is.null(input$file)){
        msg_error('Insert a file')
      } else if(!is_valid_name(input$dataset_name) || input$dataset_name %in% current_names){
        msg_error('Name invalid or already in use')
      } else {
        file <- input$file
        ext <- tools::file_ext(file$datapath)

        if (ext == 'csv' && input$radio_file_ext == 'csv') {

          tryCatch(
            { data$data <- fread(
              file = file$datapath,
              sep = input$radio_separator,
              dec = input$radio_decimal,
              check.names = T,
              nrows = Inf,
              skip = 0,
              header = input$x_csv_header)

              data$data_name <- input$dataset_name

              msg('File imported')
            },
            warning = \(w) msg_error('Check parameters'),
            error = \(e) msg_error('Check parameters')
          )

        } else if (ext == 'RDS' && input$radio_file_ext == 'RDS') {

          data_temp <- readRDS(file$datapath)

          if(data_temp |> is.data.frame()){
            data$data <- data_temp
            data$data_name <- input$dataset_name

            msg('File imported')
          } else {
            msg_error('Object must be data.frame')
          }
        } else {
          msg_error(paste('Insert a', input$radio_file_ext, 'file'))
        }
      }

    }) |> bindEvent(input$btn_import)

    observe({
      req(input$file)

        file <- input$file
        ext <- tools::file_ext(file$datapath)

        if (ext == 'csv' && input$radio_file_ext == 'csv') {
          preview_data <- readLines(file$datapath, n = 8)

          showModal(modalDialog(
            size = 'xl',
            title = 'Preview Raw File',
            HTML(paste(preview_data, collapse = '<br>'))
          ))

        } else {
          msg_error(paste('Insert a', input$radio_file_ext, 'file'))
        }
      }) |> bindEvent(input$btn_preview_raw)

    data_imported <- reactive({
      req(data$data)
      req(data$data_name)
      req(input$dataset_name)
      list('data' = data$data,
           'data_name' = data$data_name,
           # return the click integer for update the value outside the module
           # even if the name and file was used before
           'btn_click' = input$btn_import)
    })

    return(list(data_imported = data_imported))

  })
}
