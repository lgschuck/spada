
# ui --------------------------------------------------------------------------
calculate_cols_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header('Apply Function', class = 'mini-header'),
    card_body(
      uiOutput(ns('ui_var_sel')),
      textInput(ns('txt_new_name'), 'New variable name'),
      uiOutput(ns('ui_fun_sel')),
      uiOutput(ns('ui_var_groupby'))
    ),
    card_footer(btn_task(
      ns('btn_apply_fun'), 'Apply', icon('check')
    ))
  )
}

# server ----------------------------------------------------------------------
calculate_cols_server <- function(id, input_df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Reactive to get column names
    df_names <- reactive(input_df() |> names())

    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
    })

    # render variables to sel -------------------------------------------------
    output$ui_var_sel <- renderUI({
      selectInput(ns('vars_sel'), 'Variable', c('', df_names()))
    })

    # render variables group -------------------------------------------------
    output$ui_var_groupby <- renderUI({
      selectizeInput(ns('vars_groupby'), 'Group by', c('None' = NULL, df_names()),
                     multiple = T,
                     options = list(plugins = list('remove_button', 'clear_button')))
    })

    # suggest name for new variable -------------------------------------------
    observe({
      req(input$vars_sel)
      updateTextInput(session, 'txt_new_name',
                      value = paste0(input$vars_sel, '_new'))
    }) |> bindEvent(input$vars_sel)

    # render functions choices ------------------------------------------------
    selected_var_type <- reactive({
      req(input$vars_sel)
      obj_type(df$df_active[[input$vars_sel]])
    })

    output$ui_fun_sel <- renderUI({
      req(input$vars_sel)

      selectInput(ns('fun'), 'Choose a function', choices = switch(
        selected_var_type(),
        "numeric"  = math_funs,
        "char"     = char_funs,
        "date"     = date_funs,
        "logical"  = logical_funs,
        "factor"   = factor_funs,
        "complex"  = complex_funs,
        character(0)
      ))

    })

    observe({
      req(input$vars_sel, input$txt_new_name, df$df_active, input$fun)
      if (input$vars_sel |> length() == 0) {
        msg('Select at least one variable')
      } else {
        if (is_valid_name(input$txt_new_name) &&
            input$txt_new_name %notin% df_names()) {

          temp <- copy(df$df_active)

          temp[, new_var := fun(var1), by = groupby, env = list(
            new_var = input$txt_new_name,
            fun = input$fun,
            var1 = input$vars_sel,
            groupby = input$vars_groupby |> as.list()
          )]

          df$df_active <- copy(temp)
          rm(temp)

          msg('Apply function: OK')

        } else {
          msg_error('New name is not valid or already in use')
        }

      }
    }) |> bindEvent(input$btn_apply_fun)

    return(list(df_calculate_cols = reactive(df$df_active)))
  })
}
