
# ui --------------------------------------------------------------------------
output_ui <- function(id) {
  ns <- NS(id)

  card(
    card_body(fillable = F,
      uiOutput(ns('panel'))
    ),
    card_footer(
      layout_columns(
        col_widths = c(2, 2),
        btn_task(ns('btn_reset'), 'Reset'),
        downloadButton(ns('save_html'), 'Save HTML', class = 'btn-task',
                       icon = icon('download'))
      )
    )
  )

}

# server ----------------------------------------------------------------------
output_server <- function(id, output_report) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    output_list <- reactiveValues(elements = NULL)

    observe({
      output_list$elements <- output_report()
    })

    # render panel ------------------------------------------------------------
    output$panel <- renderUI({
      output_list$elements
    })

    # reset output ------------------------------------------------------------
    observe({
      output_list$elements <- list(report_card())
    }) |> bindEvent(input$btn_reset)

    # save html ---------------------------------------------------------------
    output$save_html <- downloadHandler(
      filename = function() {
        paste0('output_', format(Sys.time(), format = '%Y%m%d%H%M%S'), '.html')
      },
      content = function(file) {

        save_html(do.call('tagList', output_list$elements), file)

      }
    )

    return(list(output_file = reactive(output_list$elements)))

  })
}
