
# ui --------------------------------------------------------------------------
rename_cols_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header('Rename Columns', class = 'mini-header'),
    card_body(
      uiOutput(ns('ui_var_sel')),
      textInput(ns('txt_new_name'), 'New name'),
    ),
    card_footer(btn_task(ns('btn_rename'), 'Rename Variable', icon('check')))
  )
}

# server ----------------------------------------------------------------------
rename_cols_server <- function(id, input_df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Reactive to get column names
    df_names <- reactive(input_df() |> names())

    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
    })

    output$ui_var_sel <- renderUI({
      selectInput(ns('vars_sel'), 'Variable', c('', df_names()))
    })

    observe({
      req(input$vars_sel, input$txt_new_name, df$df_active)
      if(input$vars_sel |> length() == 0){
        msg('Select at least one variable')
      } else {
        if(is_valid_name(input$txt_new_name) &&
           input$txt_new_name %notin% df_names()){

          temp <- copy(df$df_active)

          setnames(temp, input$vars_sel, input$txt_new_name)

          df$df_active <- copy(temp)
		  rm(temp)

          msg('Rename columns: OK')
        } else {
          msg_error('New name is not valid or already in use')
        }

      }
    }) |> bindEvent(input$btn_rename)

    return(list(df_rename_cols = reactive(df$df_active)))
  })
}
