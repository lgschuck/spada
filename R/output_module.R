
# ui --------------------------------------------------------------------------
output_ui <- function(id) {
  ns <- NS(id)

  navset_card_pill(
    nav_panel('Output',
      card(
        card_body(fillable = F,
          uiOutput(ns('panel'))
        ),
        card_footer(
          layout_columns(
            col_widths = c(2, 2),
            actionButton(ns('btn_reset'), 'Reset', icon('rotate-right'), class = 'btn-task'),
            downloadButton(ns('btn_save_html'), 'Save HTML', class = 'btn-task',
                           icon = icon('download')),
            actionButton(ns('btn_save_output_session'), 'Save Output', icon('download'),
                         class = 'btn-task', ),
            actionButton(ns('btn_import_output_session'), 'Import Output', icon('upload'),
                         class = 'btn-task')
          )
        )
      )
    ),
    nav_panel('Output Meta',
      gt_output(ns('elements_meta'))
    )
  )
}

# server ----------------------------------------------------------------------
output_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output_header <- reactiveVal({
      id <- gen_element_id()
      list(
        'id' = id,
        'title' = 'Spada Output',
        'card' = report_card(id = id)
      )
    })

    # render panel ------------------------------------------------------------
    printable_output <- reactive({
      list(
        output_header()$card,
        lapply(session$userData$out$elements, function(x) x$card)
      )
    })

    output$panel <- renderUI({
      # session$userData$out$elements
      printable_output()
    })

    # render output metadata --------------------------------------------------
    output$elements_meta <- render_gt({
      data.frame(
        position = seq_along(session$userData$out$elements),
        id = sapply(session$userData$out$elements, function(x) x$id),
        title = sapply(session$userData$out$elements, function(x) x$title)
        ) |> gt()
    })

    # reset output ------------------------------------------------------------
    observe({
      showModal(modalDialog(
        title = 'Reset Output',
        'Do you want to remove all the elements?',
        easyClose = FALSE,
        size = 'l',
        footer = tagList(
          actionButton(ns('btn_cancel_reset'), 'No', icon('xmark'),
                       class = 'btn-task btn-task-cancel'),
          btn_task(ns('btn_confirm_reset'), 'Yes', icon('check'))
        )
      ))
    }) |> bindEvent(input$btn_reset)

    observe({
      session$userData$out$elements <- list()
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
        save_html(do.call('tagList', printable_output()), file)
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
          actionButton(ns('btn_cancel_save_output'), 'No', icon('xmark'),
                   class = 'btn-task btn-task-cancel'),
          btn_task(ns('btn_confirm_save_output'), 'Yes', icon('check'))
        )
      ))
    }) |> bindEvent(input$btn_save_output_session)

    # cancel save output
    observe({ removeModal() }) |> bindEvent(input$btn_cancel_save_output)

    # confirm save output
    observe({
      removeModal()

      check_dir(session$userData$conf$data_dir)
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
          actionButton(ns('btn_cancel_import_output'), 'No', icon('xmark'),
                   class = 'btn-task btn-task-cancel'),
          btn_task(ns('btn_confirm_import_output'), 'Yes', icon('check'))
        )
      ))
    }) |> bindEvent(input$btn_import_output_session)

    # cancel import output
    observe({ removeModal() }) |> bindEvent(input$btn_cancel_import_output)

    # confirm import output
    observe({
      removeModal()

      if(!file.exists(paste0(session$userData$conf$data_dir, '/output.RDS'))){
        msg_error('Output file not found')
      } else {
        check_dir(session$userData$conf$data_dir)
        temp_output <- readRDS(paste0(session$userData$conf$data_dir, '/output.RDS'))

        if(test_output_format(temp_output)){
          session$userData$out$elements <- temp_output
          msg(paste('Output imported from', session$userData$conf$data_dir), 2.5)
        } else {
          msg_error('Output in invalid format')
        }
      }
    }) |> bindEvent(input$btn_confirm_import_output)

  })
}
