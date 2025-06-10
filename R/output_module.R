
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
        btn_task(ns('btn_reset'), 'Reset', icon('rotate-right')),
        downloadButton(ns('save_html'), 'Save HTML', class = 'btn-task',
                       icon = icon('download'))
      )
    )
  )

}

# server ----------------------------------------------------------------------
output_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # render panel ------------------------------------------------------------
    output$panel <- renderUI({
      session$userData$out$elements
    })

    # reset output ------------------------------------------------------------
    observe({
      showModal(modalDialog(
        title = 'Reset Output',
        'Do you wanto to remove all the elements?',
        easyClose = FALSE,
        size = 'l',
        footer = tagList(
          actionButton(ns('btn_cancel_reset'), 'Cancel', icon = icon('xmark')),
          actionButton(ns('btn_confirm_reset'), 'Confirm', icon = icon('check'))
        )
      ))
    }) |> bindEvent(input$btn_reset)

    observe({
      session$userData$out$elements <- list(report_card())
      removeModal()
    }) |> bindEvent(input$btn_confirm_reset)

    observe({
      removeModal()
    }) |> bindEvent(input$btn_cancel_reset)

    # save html ---------------------------------------------------------------
    output$save_html <- downloadHandler(
      filename = function() {
        paste0('output_', format(Sys.time(), format = '%Y%m%d%H%M%S'), '.html')
      },
      content = function(file) {

        save_html(do.call('tagList', session$userData$out$elements), file)

      }
    )

  })
}
