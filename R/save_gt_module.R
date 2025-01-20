
# ui --------------------------------------------------------------------------
save_gt_ui <- function(id) {
  ns <- NS(id)

  dropdownButton(
    inputId = ns('drop_btn'),
    label = 'Save',
    tagList(
      h3('Save table'),
      textInput(ns('file_name'), 'File name', value = 'table'),
      radioGroupButtons(ns('radio_format'), 'Format',
                        c('html', 'rtf', 'docx'), size = 'sm', individual = T),
      downloadButton(ns('down_handler'),
                     'Save table', icon('download')),
      br()),
    circle = F, size = 'sm', icon = icon('download')
  )
}

# server ----------------------------------------------------------------------
save_gt_server <- function(id, input_table) {
  moduleServer(id, function(input, output, session) {

    output$down_handler <- downloadHandler(

      filename = function() {
        req(input_table())
        paste(input$file_name,
              switch(input$radio_format,
                    html = '.html', rtf = '.rtf', docx = '.docx'))
      },
      content = function(file) {
        req(input_table())
        gtsave(input_table(), file)
      }
    )
  })
}
