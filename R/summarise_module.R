
# ui --------------------------------------------------------------------------
summarise_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Summarise', class = 'mini-header'),
      card_body(
        selectInput(ns('dt_sel'), 'Dataset', NULL),
        selectizeInput(
          ns('vars_sel'), 'Variables', NULL,
          multiple = T,
          options = list(plugins = list('remove_button', 'clear_button')),
          width = '80%'
        ),
        selectInput(ns('fun_sel'), 'Function', summarise_functions),
        textInput(ns('txt_new_dt_name'), 'New Dataset Name', placeholder = 'new_dataset'),
      ),
      card_footer(btn_task(ns('btn_summarise'), 'Summarise', icon('check')))
    ),
    card()
  )
}

# server ----------------------------------------------------------------------
summarise_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  dt_names <- reactive(session$userData$dt$dt |> names())

	  vars_names <- reactive({
      req(dt_names())
	    req(input$dt_sel)
      session$userData$dt$dt[[input$dt_sel]] |> names()
    })

    # datasets ------------------------------
    observe({
      req(dt_names())
      updateSelectizeInput(
        session,
        'dt_sel',
        choices = c(dt_names())
      )
    }) |> bindEvent(dt_names())

    # variables ------------------------------
    observe({
      req(vars_names())
      updateSelectInput(
        session,
        'vars_sel',
        choices = c('', vars_names())
      )
    }) |> bindEvent(vars_names())


    observe({
      if(!isTruthy(input$dt_sel)){
        msg('Select at least one variable')
      } else if(!isTruthy(input$vars_sel)){
        msg('Select at least one variable')
      } else {

        if(is_valid_name(input$txt_new_dt_name) &&
           input$txt_new_dt_name %notin% dt_names()){

          temp <- copy(session$userData$dt$dt[[input$dt_sel]])

          temp <- summarise_dt(temp, input$fun_sel, input$vars_sel)

          append_dt(session, temp, input$txt_new_dt_name)

          # update metadata ----------------------
          append_meta(session, temp |> df_info(), input$txt_new_dt_name)

          rm(temp)

          msg('Summarise: OK')
        } else {
          msg_error('New name is not valid or already in use')
        }

      }
    }) |> bindEvent(input$btn_summarise)

  })
}
