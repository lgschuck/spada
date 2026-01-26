
# ui --------------------------------------------------------------------------
order_rows_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header('Order Rows', class = 'mini-header'),
    card_body(
      selectizeInput(
        ns('vars_rows'),
        'Order by',
        NULL,
        multiple = T,
        options = list(plugins = list('remove_button', 'clear_button')),
        width = '80%'
      ),
      selectizeInput(
        ns('vars_descending'),
        list('Vars in descending order', bs_icon('info-circle')) |>
          ttip('If not informed, the order will be Ascending', PLACE = 'right'),
        '',
        multiple = T,
        options = list(plugins = list("remove_button", "clear_button")),
        width = '80%'
      ),
      radioButtons(ns('radio_nas'),
                   "NA's placement",
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

    df_names <- reactive(get_act_dt(session) |> names())

    observe({
      req(df_names())
      updateSelectizeInput(
        session,
        'vars_rows',
        choices = c('', df_names())
      )
    }) |> bindEvent(df_names())

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

        temp <- copy(get_act_dt(session))

        setorderv(temp, cols = input$vars_rows,
                  order = rows_position,
                  na.last = if(input$radio_nas == 'last'){
                    TRUE
                  } else if (input$radio_nas == 'first'){
                    FALSE
                  } else { FALSE }
        )

        update_act_dt(session, copy(temp), FALSE)
        rm(temp)

        msg('Reordering Rows: OK')
      }
    }) |> bindEvent(input$btn_order_rows)

  })
}
