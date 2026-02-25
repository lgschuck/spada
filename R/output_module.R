
# ui --------------------------------------------------------------------------
output_ui <- function(id) {
  ns <- NS(id)

  navset_card_pill(
    nav_panel(
      'Output',
      card(
        card_body(fillable = F, uiOutput(ns('panel'))),
        card_footer(
          fluidRow(
            column(1, checkboxInput(ns('x_flip'), 'Flip', T) |>
                     ttip('Flip upside down'),
                   style = 'margin-top: 28px'),
            column(
              3,
              radioGroupButtons(
                ns('sel_show_output'),
                'Show',
                c('All' = Inf,
                  'Last 1' = 1,
                  'Last 3' = 3,
                  'Last 5' = 5,
                  'None' = -Inf
                ),
                selected = 1,
                size = 'sm',
                individual = T
              )
            ),
            column(
              8,
              layout_columns(
                col_widths = c(3, 3, 3, 3),
                actionButton(ns('btn_reset'), 'Reset', icon('rotate-right'),
                             class = 'btn-task'),
                downloadButton(
                  ns('btn_save_html'),
                  'Save HTML',
                  class = 'btn-task',
                  icon = icon('download')
                ),
                actionButton(
                  ns('btn_save_output_session'),
                  'Save Output',
                  icon('download'),
                  class = 'btn-task'
                ),
                actionButton(
                  ns('btn_import_output_session'),
                  'Import Output',
                  icon('upload'),
                  class = 'btn-task'
                )
              ),
              style = 'margin-top: 28px'
            )
          ),
          div(style = 'margin-bottom: -8px !important;')
        )
      )
    ),
    nav_panel('Output Meta', gt_output(ns('elements_meta'))))
}

# server ----------------------------------------------------------------------
output_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # render panel ------------------------------------------------------------
    task_printable_out <- ExtendedTask$new(function(accordion,
                                                accordion_panel,
                                                div_fun = div,
                                                printable_report_card,
                                                elements,
                                                sel_show_output) {

      mirai({

        if (is.null(elements) || length(elements) == 0) {
          list()
        } else {

          len <- length(elements)

          lapply(seq_along(elements), function(i) {

            x <- elements[[i]]

            div(
              style = 'margin-bottom: 8px;',
              accordion(
                open = if (i > len - as.numeric(sel_show_output)) TRUE else FALSE,
                multiple = TRUE,
                accordion_panel(
                  paste(i, '-', x$title),
                  printable_report_card(x$btn, x$card)
                )
              )
            )
          })
        }
      },
      accordion = accordion,
      accordion_panel = accordion_panel,
      div = div,
      printable_report_card = printable_report_card,
      elements = elements,
      sel_show_output = sel_show_output
      )
    })

    observe({
      req(session$userData$out$elements)
      req(input$sel_show_output)

        task_printable_out$invoke(
          accordion = accordion,
          accordion_panel = accordion_panel,
          div = div,
          printable_report_card = printable_report_card,
          elements = session$userData$out$elements,
          sel_show_output = input$sel_show_output
        )
    }) |> bindEvent(session$userData$out$elements, input$sel_show_output)

    output$panel <- renderUI({
      req(task_printable_out$result())

      if (input$x_flip) {
        tagList(task_printable_out$result() |> rev())
      } else {
        tagList(task_printable_out$result())
      }
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

        savable_output <- list(
          report_card(),
          lapply(
            session$userData$out$elements,
            function(x){ printable_report_card(NULL, x$card, NULL) }
          )
        )

        doc <- tags$html(
          output_export_css, tags$body(savable_output)
        )

        save_html(doc, file)
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
      qs_save(session$userData$out$elements,
              paste0(session$userData$conf$data_dir, '/output.qs2'))

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

      if(!file.exists(paste0(session$userData$conf$data_dir, '/output.qs2'))){
        msg_error('Output file not found')
      } else {
        check_dir(session$userData$conf$data_dir)
        temp_output <- qs_read(paste0(session$userData$conf$data_dir, '/output.qs2'))

        if(test_output_format(temp_output)){

          temp_output <- lapply(temp_output, function(x) {

            id <- gen_element_id()
            x$id <- id
            btn_id <- paste0("btn_xout_", id)

            x$btn <- actionButton(
              ns(btn_id),
              '',
              icon('x'),
              class = 'micro-btn-cancel'
            )

            observe({
              session$userData$out$elements[[id]] <- NULL
            }) |> bindEvent(input[[btn_id]], once = TRUE)

            return(x)
          })
          # rename items of list with new ids
          names(temp_output) <- vapply(temp_output, function(x) x$id, character(1))

          session$userData$out$elements <- temp_output
          msg(paste('Output imported from', session$userData$conf$data_dir), 2.5)
        } else {
          msg_error('Output in invalid format')
        }
      }
    }) |> bindEvent(input$btn_confirm_import_output)

  })
}
