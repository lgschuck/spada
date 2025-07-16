
# ui --------------------------------------------------------------------------
select_cols_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header('Select Columns', class = 'mini-header'),
    card_body(
      uiOutput(ns('ui_var_sel')),
      radioButtons(ns('radio_var_sel'), NULL,
                   c('Drop' = 'drop', 'Keep' = 'keep'), inline = T)
    ),
    card_footer(btn_task(ns('btn_sel'), 'Apply selection', icon('check')))
  )
}

# server ----------------------------------------------------------------------
select_cols_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    df_names <- reactive(get_act_dt(session) |> names())

    df <- reactiveValues()
    observe({
      df$df_active <- get_act_dt(session)
    })

    output$ui_var_sel <- renderUI(
      selectizeInput(ns('vars_sel'), 'Variable', c('', df_names()), multiple = T,
                     options = list(plugins = list('remove_button', 'clear_button'))
                     )
    )

    observe({
      if(input$vars_sel |> length() == 0){
        msg('Select at least one variable')
      } else {

        temp <- copy(df$df_active)

        if(input$radio_var_sel == 'keep') {
          temp <- subset(temp, select = input$vars_sel)
          msg('Select columns: OK')
        } else if (input$radio_var_sel == 'drop'){
          if(all(df_names() %in% input$vars_sel)){
            msg('Leave at least 1 variable')
          } else {
            temp <- subset(
              temp,
              select = setdiff(df_names(), input$vars_sel))
            msg('Select columns: OK')

          }
        }

        df$df_active <- copy(temp)
        rm(temp)

      }
    }) |> bindEvent(input$btn_sel)

    # update active dataset ---------------------------------------------------
    observe({
      req(df$df_active)
      update_act_dt(session, df$df_active)
    })

  })
}
