
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
order_rows_server <- function(id, input_df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    df_names <- reactive(input_df() |> names())

    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
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

        setorderv(df$df_active, cols = input$vars_rows,
                  order = rows_position,
                  na.last = if(input$radio_nas == 'last'){
                    TRUE
                  } else if (input$radio_nas == 'first'){
                    FALSE
                  } else { FALSE }
        )
        msg('Reordering Rows: OK')
      }
    }) |> bindEvent(input$btn_order_rows)

    return(list(df_order_rows = reactive(df$df_active),
                btn_order_rows = reactive(input$btn_order_rows)))
  })
}
