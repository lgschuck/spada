
# ui --------------------------------------------------------------------------
order_cols_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header('Order Columns', class = 'mini-header'),
    card_body(
      selectizeInput(
        ns('vars_cols'),
        'Variables to move',
        NULL,
        multiple = T,
        options = list(plugins = list('remove_button', 'clear_button')),
        width = '80%'
      ),
      radioButtons(ns('radio_cols'),
                   NULL,
                   c('Before' = 'before', 'After' = 'after'),
                   inline = T),
      selectInput(
        ns('vars_rest'),
        list('Other Variables', bs_icon('info-circle')) |>
          ttip(
            'If not informed, the Other Variables will be placed at the end',
            PLACE = 'right'
          ),
        '',
        width = '80%'
      )
    ),
    card_footer(
      btn_task(ns('btn_order_cols'), 'Order Columns', icon('arrow-right-arrow-left')),
    )
  )
}

# server ----------------------------------------------------------------------
order_cols_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    df_names <- reactive(get_act_dt(session) |> names())

    observe({
      req(df_names())
      updateSelectizeInput(
        session,
        'vars_cols',
        choices = c('', df_names())
      )
    }) |> bindEvent(df_names())

    # rest of vars to order
    observe({
      updateSelectInput(
        session,
        'vars_rest',
        label = 'Other Variables',
        choices = df_names()[df_names() %notin% input$vars_cols]
      )
    }) |> bindEvent(input$vars_cols)

    # btn order cols ---------------------------
    observe({
      if(!isTruthy(input$vars_cols)){
        msg('Choose at least one variable')
      } else {

        temp <- copy(get_act_dt(session))

        if(all(df_names() %in% input$vars_cols) || input$vars_rest == ''){
          setcolorder(temp, input$vars_cols)
        } else if(input$radio_cols == 'before'){
          setcolorder(temp, input$vars_cols, before = input$vars_rest)
        } else if (input$radio_cols == 'after') {
          setcolorder(temp, input$vars_cols, after = input$vars_rest)
        }

        update_act_dt(session, copy(temp))
        rm(temp)

        msg('Reordering Variables: OK')
      }
    }) |> bindEvent(input$btn_order_cols)

  })
}
