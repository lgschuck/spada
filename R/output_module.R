
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
        downloadButton(ns('btn_save_html'), 'Save HTML', class = 'btn-task',
                       icon = icon('download')),
        btn_task(ns('btn_save_output_session'), 'Save Output',
                 class = 'btn-task', icon('download')),
        btn_task(ns('btn_import_output_session'), 'Import Output',
                 class = 'btn-task', icon('upload'))
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
        'Do you want to remove all the elements?',
        easyClose = FALSE,
        size = 'l',
        footer = tagList(
          actionButton(ns('btn_cancel_reset'), 'No',
                       icon = icon('xmark'), class = 'btn-task'),
          actionButton(ns('btn_confirm_reset'), 'Yes',
                       icon = icon('check'), class = 'btn-task')
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
    output$btn_save_html <- downloadHandler(
      filename = function() {
        paste0('output_', format(Sys.time(), format = '%Y%m%d%H%M%S'), '.html')
      },
      content = function(file) {

        save_html(do.call('tagList', session$userData$out$elements), file)

      }
    )

    # save session output -----------------------------------------------------
    observe({

      showModal(modalDialog(
        title = 'Save Output',
        'Do you want to save the current Output? This will overwrite the previous one.',
        easyClose = FALSE,
        size = 'l',
        footer = tagList(
          actionButton(ns('btn_cancel_save_output'), 'No',
                       icon = icon('xmark'), class = 'btn-task'),
          actionButton(ns('btn_confirm_save_output'), 'Yes',
                       icon = icon('check'), class = 'btn-task')
        )
      ))
    }) |> bindEvent(input$btn_save_output_session)

    # cancel save output
    observe({ removeModal() }) |> bindEvent(input$btn_cancel_save_output)

    # confirm save output
    observe({
      removeModal()
      saveRDS(session$userData$out$elements,
              paste0(session$userData$conf$data_dir, '/output.RDS'))

      msg(paste('Output saved in', session$userData$conf$data_dir), 2.5)
    }) |> bindEvent(input$btn_confirm_save_output)

    # import session output ---------------------------------------------------
    observe({
      showModal(modalDialog(
        title = 'Import Output',
        'Do you want to import the previous saved Output? This will erase
        the current Output elements.',
        easyClose = FALSE,
        size = 'l',
        footer = tagList(
          actionButton(ns('btn_cancel_import_output'), 'No',
                       icon = icon('xmark'), class = 'btn-task'),
          actionButton(ns('btn_confirm_import_output'), 'Yes',
                       icon = icon('check'), class = 'btn-task')
        )
      ))
    }) |> bindEvent(input$btn_import_output_session)

    # cancel import output
    observe({ removeModal() }) |> bindEvent(input$btn_cancel_import_output)

    # confirm import output
    observe({
      removeModal()
      temp_output <- readRDS(paste0(session$userData$conf$data_dir, '/output.RDS'))

      if(test_output_format(temp_output)){
        session$userData$out$elements <- temp_output
        msg(paste('Output imported from', session$userData$conf$data_dir), 2.5)
      } else {
        msg_error('Output in invalid format')
      }
    }) |> bindEvent(input$btn_confirm_import_output)

  })
}
