
# ui --------------------------------------------------------------------------
export_file_ui <- function(id) {
  ns <- NS(id)
    layout_column_wrap(
      card(
        fluidRow(
          column(3, textInput(ns('file_name'), 'File name', value = 'dataset')),
          column(3, radioButtons(ns('radio_format'), 'File format',
                                 c('csv', 'RDS', 'RDS Compressed'), inline = T))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'csv'", ns('radio_format')),
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
        card_footer(downloadButton(ns('down_handler'),
                                   'Export Active dataset', icon('download')))
      )
    )
}

# server ----------------------------------------------------------------------
export_file_server <- function(id, df_active) {
  moduleServer(id, function(input, output, session) {
    output$down_handler <- downloadHandler(

      filename = function() {
        paste(input$file_name,
              if(input$radio_format == 'csv'){
                '.csv'
              } else if (input$radio_format %in% c('RDS', 'RDS Compressed')){
                '.RDS'
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
          saveRDS(df_active(), file, compress = F)
        } else if (input$radio_format == 'RDS Compressed') {
          saveRDS(df_active(), file, compress = T)
        }
      }
    )
  })
}
