
# ui --------------------------------------------------------------------------
insert_output_ui <- function(id) {
  ns <- NS(id)

  btn_task(ns('btn_add_output'), 'Add to output', icon('plus'))
}

# server ----------------------------------------------------------------------
insert_output_server <- function(id, input_element) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output_element <- reactiveVal(NULL)

    # add output
    observe({
      req(input_element())

      showModal(
        modalDialog(
          title = 'Add element to Output',
          size = 'l',
          textInput(ns('output_title'), 'Title'),
          textAreaInput(
            ns('output_annot'),
            'Annotation',
            width = '90%',
            rows = 10,
            resize = 'both'
          ),
          footer = tagList(
            actionButton(ns('btn_cancel_add_output'), 'Cancel',
                         icon = icon('xmark'), class = 'btn-task'),
            actionButton(ns('btn_confirm_add_output'), 'Submit',
                         icon = icon('check'), class = 'btn-task')
          )
        )
      )

    }) |> bindEvent(input$btn_add_output)

    # cancel_add_output
    observe({
      removeModal()
    }) |> bindEvent(input$btn_cancel_add_output)

    # confirm add output
    observe({
      removeModal()

      output_element(report_card(input$output_title, input$output_annot, input_element()))

      msg('Added to output', DURATION = 1)

    }) |> bindEvent(input$btn_confirm_add_output)

    # return
    return(list(output_element = reactive(output_element())))
  })
}
