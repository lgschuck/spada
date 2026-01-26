
# ui --------------------------------------------------------------------------
summarise_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Summarise', class = 'mini-header'),
      card_body(
        selectizeInput(
          ns('vars_sel'), 'Variables', NULL,
          multiple = T,
          options = list(plugins = list('remove_button', 'clear_button')),
          width = '80%'
        ),
        selectInput(ns('fun_sel'), 'Function', summarise_functions),
        radioButtons(ns('radio_overwrite'), NULL,
                     c('Overwrite' = 'overwrite', 'New' = 'new'), inline = T),
        conditionalPanel(
          condition = "input.radio_overwrite == 'new'", ns = ns,
          textInput(ns('txt_new_dt_name'), 'New Dataset Name', placeholder = 'new_dataset')
        )
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

	  vars_names <- reactive(get_act_dt(session) |> names())

    # variables ------------------------------
    observe({
      req(vars_names())
      updateSelectInput(
        session,
        'vars_sel',
        choices = c('', vars_names())
      )
    }) |> bindEvent(vars_names())

	  # summarise event ---------
    observe({
      if(!isTruthy(input$vars_sel)){
        msg('Select at least one variable')
      } else if (input$radio_overwrite == 'new' &&
          (!is_valid_name(input$txt_new_dt_name) ||
             input$txt_new_dt_name %in% session$userData$dt_names()))
      {
        msg_error('New name is not valid or already in use')
      } else {
        temp <- copy(get_act_dt(session))

        temp <- summarise_dt(temp, input$fun_sel, input$vars_sel)

        # overwrite -------
        if(input$radio_overwrite == 'overwrite'){

          update_act_dt(session, temp)

        } else if(input$radio_overwrite == 'new'){

          append_dt(session, temp, input$txt_new_dt_name)
          append_meta(session, temp |> df_info(), input$txt_new_dt_name)

        }
        rm(temp)
        msg('Summarise: OK')
      }

    }) |> bindEvent(input$btn_summarise)

  })
}
