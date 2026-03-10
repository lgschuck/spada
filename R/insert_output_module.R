
# ui --------------------------------------------------------------------------
insert_output_ui <- function(id) {
  ns <- NS(id)

  btn_task(ns('btn_add_output'), 'Add to output', icon('plus'))
}

# server ----------------------------------------------------------------------
insert_output_server <- function(id, input_element, element_title = 'Title') {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    is_large_data_frame <- reactive({
      if('gt_tbl' %in% (input_element() |> class()) &&
         'data.frame' %in% (input_element()$`_data` |> class()) &&
         ((input_element()$`_data` |> nrow()) > 250 ||
          (input_element()$`_data` |> ncol()) > 250)) TRUE else FALSE
    })

    # add output -----------------------------
    observe({
      req(input_element())

      showModal(
        modalDialog(
          title = 'Add element to Output',
          size = 'l',
          tagList(
            if (is_large_data_frame()) {
              div(
                class = 'alert alert-warning',
                icon('triangle-exclamation'),
                'Inserting an extensive data.frame in the Output may cause performance degradation'
              )
            },
            textInput(ns('output_title'), 'Title', value = element_title),
            textAreaInput(
              ns('output_annot'),
              'Annotation',
              width = '90%',
              rows = 10,
              resize = 'both'
            )
          ),
          footer = tagList(
            actionButton(ns('btn_cancel_add_output'), 'Cancel',
                         icon = icon('xmark'), class = 'btn-task btn-task-cancel'),
            actionButton(ns('btn_confirm_add_output'), 'Submit',
                         icon = icon('check'), class = 'btn-task')
          )
        )
      )

    }) |> bindEvent(input$btn_add_output)

    # cancel_add_output ----------------------
    observe({
      removeModal()
    }) |> bindEvent(input$btn_cancel_add_output)

    # confirm add output ---------------------
    observe({
      if(!isTruthy(input$output_title) || trimws(input$output_title) == ''){
        msg('Insert a Title', DURATION = 1)
      } else {
        removeModal()

        id <- gen_element_id()
        btn_xid <- paste0('btn_xout_', id)
        btn_eid <- paste0('btn_eout_', id)

        btn_x <- actionButton(
          ns(btn_xid),
          '',
          icon('x'),
          class = 'micro-btn-cancel'
        )

        btn_e <- actionButton(
          ns(btn_eid),
          '',
          icon('pen-to-square'),
          class = 'micro-btn-cancel'
        )

        output_card <- report_card(input$output_title, input$output_annot, input_element())

        # delete event
        observe({
          session$userData$out$elements[[id]] <- NULL
        }) |> bindEvent(input[[btn_xid]], once = T)

        # edit event
        observe({
          session$userData$out_edit_trigger(id)
        }) |> bindEvent(input[[btn_eid]])

        # insert element in the output
        session$userData$out$elements[[id]] <- list(
          'id' = id,
          'title' = input$output_title,
          'annotation' = input$output_annot,
          'element' = input_element(),
          'card' = output_card,
          'btn_x' = btn_x,
          'btn_e' = btn_e
        )
        msg('Added to output', DURATION = 1)
      }

    }) |> bindEvent(input$btn_confirm_add_output)

  })
}
