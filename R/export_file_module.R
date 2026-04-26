
# ui --------------------------------------------------------------------------
export_file_ui <- function(id) {
  ns <- NS(id)
    layout_column_wrap(
      card(
        fluidRow(
          column(3, textInput(ns('file_name'), 'File name', value = 'dataset')),
          column(3, radioButtons(ns('radio_format'), 'File format',
                                 c('csv', 'qs2', 'RDS', 'sav', 'xlsx'), inline = T))
        ),
        conditionalPanel(
          condition = "input.radio_format == 'csv'", ns = ns,
          card(
            card_header('Csv Parameters', class = 'mini-header'),
            card_body(
              checkboxInput(ns('x_rownames'), 'Save row names'),
              fluidRow(
                column(3, radioButtons(ns('radio_separator'), 'Separator',
                             c('Semicolon' = ';', 'Comma' = ','), inline = T)),
                column(3, radioButtons(ns('radio_decimal'), 'Decimal mark',
                       c('Dot' = '.', 'Comma' = ','), inline = T))
              ),
              fluidRow(
                column(3, textInput(ns('txt_na'), 'Missing (NA) substitute', value = '')),
                column(3, radioButtons(ns('radio_scientific'), 'Scientific notation',
                             c('No' = 999999999, 'Allow' = 0), inline = T))
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.radio_format == 'RDS'", ns = ns,
          card(
            card_header('RDS Parameters', class = 'mini-header'),
            card_body(
              checkboxInput(ns('checkbox_rds_compress'), 'Compress')
            ),
          )
        ),
        conditionalPanel(
          condition = "input.radio_format == 'sav'", ns = ns,
          card(
            card_header('Sav Parameters', class = 'mini-header'),
            card_body(
              radioButtons(ns('radio_sav_compress'), 'Compress',
                           c('Byte' = 'byte', 'None' = 'none', 'Zsav' = 'zsav'),
                           inline = T)
            ),
          )
        ),
        card_footer(downloadButton(
          ns('down_handler'),
          'Export Active dataset',
          class = 'btn-task',
          icon = icon('download')
        ))
      )
    )
}

# server ----------------------------------------------------------------------
export_file_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    act_dt <- reactive({ get_act_dt(session) })

    output$down_handler <- downloadHandler(

      filename = function() {

        paste0(
          input$file_name,
          switch(
            input$radio_format,
            'csv' = '.csv',
            'qs2' = '.qs2',
            'RDS' = '.RDS',
            'RDS Compressed' = '.RDS',
            'sav' = '.sav',
            'xlsx' = '.xlsx',
            ''
          )
        )
      },
      content = function(file) {

        dt <- act_dt()

        if(input$radio_format == 'csv'){
          fwrite(dt, file,
                 row.names = input$x_rownames,
                 sep = input$radio_separator,
                 dec = input$radio_decimal,
                 na = input$txt_na,
                 scipen = as.integer(input$radio_scientific)
          )
        } else if (input$radio_format == 'qs2'){
          qs_save(dt, file)
        } else if (input$radio_format == 'RDS'){
          saveRDS(dt, file, compress = input$checkbox_rds_compress)
        } else if (input$radio_format == 'sav') {
          write_sav(dt, file, compress = input$radio_sav_compress)
        } else if (input$radio_format == 'xlsx') {

          if (nrow(dt) <= 1048575 && ncol(dt) <= 16384) {
            write_xlsx(dt, file, format_headers = F)
          } else {

            rows <- min(1048575, nrow(dt))
            cols <- min(16384, ncol(dt))
            showModal(
              modalDialog(
                title = 'xlsx Limits',
                paste(
                  'xlsx format supports 1,048,576 rows and 16,384 columns.',
                  'Exporting', f_int(rows), 'rows (and the headers) and',
                  f_int(cols), 'columns.'
                ),
                easyClose = TRUE,
                footer = modalButton('Dismiss')
              )
            )

            write_xlsx(dt[1:rows, 1:cols], file, format_headers = F)
          }
        }
      }
    )
  })
}
