
# ui --------------------------------------------------------------------------
order_rows_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header('Order Rows', class = 'mini-header'),
    card_body(
      uiOutput(ns('ui_var_rows')),
      selectizeInput(ns('vars_descending'),
                  list('Vars in descending order', bs_icon('info-circle')) |>
                    ttip('If not informed, the order will be Ascending', PLACE = 'right'),
                    '', multiple = T,
                  options = list(plugins = list("remove_button", "clear_button"))
                  ),
      radioButtons(ns('radio_nas'), "NA's placement",
                   c('Last' = 'last', 'First' = 'first'),
                   inline = T),
    ),
    card_footer(
      btn_task(ns('btn_order_rows'), 'Order Rows', icon('shuffle')),
    )
  )
}

# server ----------------------------------------------------------------------
order_rows_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    # Reactive to get column names
    df_names <- reactive(session$userData$df$act |> names())

    df <- reactiveValues()
    observe({
      df$df_active <- session$userData$df$act
    })

    output$ui_var_rows <- renderUI(
      selectizeInput(ns('vars_rows'), 'Order by', c('', df_names()), multiple = T,
                     options = list(plugins = list('remove_button', 'clear_button'))
                     )
    )

    # vars in descending order
    observe({
      updateSelectizeInput(
        session,
        'vars_descending',
        label = 'Descending Order',
        choices = input$vars_rows
      )
    }) |> bindEvent(input$vars_rows)

    # btn order rows ---------------------------
    observe({
      if(!isTruthy(input$vars_rows)){
        msg('Choose at least one variable')
      } else {
        rows_position <- rep(1, input$vars_rows |> length())

        rows_position[which(input$vars_rows %in% input$vars_descending)] <- -1

        temp <- copy(df$df_active)
        setorderv(temp, cols = input$vars_rows,
                  order = rows_position,
                  na.last = if(input$radio_nas == 'last'){
                    TRUE
                  } else if (input$radio_nas == 'first'){
                    FALSE
                  } else { FALSE }
        )

        df$df_active <- copy(temp)
        rm(temp)

        msg('Reordering Rows: OK')
      }
    }) |> bindEvent(input$btn_order_rows)

    # update active dataset ---------------------------------------------------
    observe({
      req(df$df_active)
      session$userData$df$act <- df$df_active
    })

  })
}
