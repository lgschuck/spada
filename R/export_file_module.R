
# ui --------------------------------------------------------------------------
export_file_ui <- function(id) {
  ns <- NS(id)
    layout_column_wrap(
      card(
        fluidRow(
          column(3, textInput(ns('file_name'), 'File name', value = 'dataset')),
          column(3, radioButtons(ns('radio_format'), 'File format',
                                 c('csv', 'RDS', 'sav'), inline = T))
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
export_file_server <- function(id, df_active) {
  moduleServer(id, function(input, output, session) {
    output$down_handler <- downloadHandler(

      filename = function() {
        paste0(input$file_name,
              if(input$radio_format == 'csv'){
                '.csv'
              } else if (input$radio_format %in% c('RDS', 'RDS Compressed')){
                '.RDS'
              } else if (input$radio_format %in% c('sav')){
                '.sav'
              })
      },
      content = function(file) {
        if(input$radio_format == 'csv'){
          fwrite(df_active(), file,
                 row.names = input$x_rownames,
                 sep = input$radio_separator,
                 dec = input$radio_decimal,
                 na = input$txt_na,
                 scipen = as.integer(input$radio_scientific)
          )
        } else if (input$radio_format == 'RDS'){
          saveRDS(df_active(), file, compress = input$checkbox_rds_compress)
        } else if (input$radio_format == 'sav') {
          write_sav(df_active(), file, compress = input$radio_sav_compress)
        }
      }
    )
  })
}
