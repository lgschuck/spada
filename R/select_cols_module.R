
# ui --------------------------------------------------------------------------
select_cols_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header('Select Columns', class = 'mini-header'),
    card_body(
      selectizeInput(
        ns('vars_sel'),
        'Variable',
        NULL,
        multiple = T,
        options = list(plugins = list('remove_button', 'clear_button'))
      ),

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

    observe({
      req(df_names())
      updateSelectizeInput(
        session,
        'vars_sel',
        choices = c('', df_names())
      )
    }) |> bindEvent(df_names())

    observe({
      if(input$vars_sel |> length() == 0){
        msg('Select at least one variable')
      } else {

        temp <- copy(get_act_dt(session))

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

        update_act_dt(session, copy(temp))

        rm(temp)

        session$userData$dt$data_changed(session$userData$dt$data_changed() + 1)
      }
    }) |> bindEvent(input$btn_sel)

  })
}
