
# ui --------------------------------------------------------------------------
order_cols_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header('Order Columns', class = 'mini-header'),
    card_body(
      uiOutput(ns('ui_var_cols')),
      radioButtons(ns('radio_cols'), NULL,
                   c('Before' = 'before', 'After' = 'after'),
                   inline = T),
      selectInput(ns('vars_rest'),
                  list('Other Variables', bs_icon('info-circle')) |>
                    ttip('If not informed, the Other Variables will be placed at the end',
                         PLACE = 'right'),
                  '')
    ),
    card_footer(
      btn_task(ns('btn_order_cols'), 'Order Columns', icon('arrow-right-arrow-left')),
    )
  )
}

# server ----------------------------------------------------------------------
order_cols_server <- function(id, input_df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Reactive to get column names
    df_names <- reactive(input_df() |> names())

    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
    })

    output$ui_var_cols <- renderUI(
      selectizeInput(ns('vars_cols'), 'Variables to move', c('', df_names()),
                     multiple = T,
                     options = list(plugins = list('remove_button', 'clear_button'))
                     )
    )

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

        temp <- copy(df$df_active)

        if(all(df_names() %in% input$vars_cols) || input$vars_rest == ''){
          setcolorder(temp, input$vars_cols)
        } else if(input$radio_cols == 'before'){
          setcolorder(temp, input$vars_cols, before = input$vars_rest)
        } else if (input$radio_cols == 'after') {
          setcolorder(temp, input$vars_cols, after = input$vars_rest)
        }

        df$df_active <- copy(temp)
        rm(temp)

        msg('Reordering Variables: OK')
      }
    }) |> bindEvent(input$btn_order_cols)

    return(list(df_order_cols = reactive(df$df_active)))
  })
}
