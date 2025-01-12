
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
select_cols_server <- function(id, input_df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    df_names <- reactive(input_df() |> names())

    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
    })

    output$ui_var_sel <- renderUI(
      selectInput(ns('vars_sel'), 'Variable', c('', df_names()), multiple = T)
    )

    observe({
      if(input$vars_sel |> length() == 0){
        msg('Select at least one variable')
      } else {
        if(input$radio_var_sel == 'keep') {
          df$df_active <- subset(df$df_active, select = input$vars_sel)
          msg('Select columns: OK')
        } else if (input$radio_var_sel == 'drop'){
          if(all(df_names() %in% input$vars_sel)){
            msg('Leave at least 1 variable')
          } else {
            df$df_active <- subset(
              df$df_active,
              select = setdiff(df_names(), input$vars_sel))
            msg('Select columns: OK')
          }
        }
      }
    }) |> bindEvent(input$btn_sel)

    return(list(df_sel_cols = reactive(df$df_active)))
  })
}
