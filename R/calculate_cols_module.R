
# ui --------------------------------------------------------------------------
calculate_cols_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Apply Function', class = 'mini-header'),
      card_body(
        uiOutput(ns('ui_var_sel')),
        selectInput(ns('fun'), 'Choose a function', character(0)),
        textInput(ns('txt_new_name'), 'New variable name'),
        uiOutput(ns('ui_var_groupby_fun'))
      ),
      card_footer(btn_task(
        ns('btn_apply_fun'), 'Apply', icon('check')
      ))
    ),
    card(
      card_header('Freehand', class = 'mini-header'),
      card_body(
        textInput(ns('new_name_free'), 'New var name', placeholder = 'new_var'),
        textAreaInput(ns('txt_code_input'), 'Input code', width = '800px',
                      height = '200px'),
        uiOutput(ns('ui_var_groupby_free'))
      ),
      card_footer(btn_task(
        ns('btn_apply_free'), 'Apply', icon('check')
      ))
    )
  )
}

# server ----------------------------------------------------------------------
calculate_cols_server <- function(id, input_df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
    })

    # Reactive to get column names
    df_names <- reactive(df$df_active |> names())

    # render variables to sel -------------------------------------------------
    output$ui_var_sel <- renderUI({
      selectInput(ns('vars_sel'), 'Variable', c('', df_names()))
    })

    # render variables group -------------------------------------------------
    output$ui_var_groupby_fun <- renderUI({
      selectizeInput(ns('vars_groupby_fun'), 'Group by', df_names(),
                     multiple = T,
                     options = list(plugins = list('remove_button', 'clear_button')))
    })

    # suggest name for new variable -------------------------------------------
    observe({
      req(input$vars_sel, input$fun)
      updateTextInput(session, 'txt_new_name',
                      value = paste0(input$vars_sel, '_', input$fun))
    }) |> bindEvent(input$vars_sel, input$fun)

    # render functions choices ------------------------------------------------
    selected_var_type <- reactive({
      req(input$vars_sel)
      obj_type(df$df_active[[input$vars_sel]])
    })

    observe({
      req(input$vars_sel)

      updateSelectInput(
        session,
        'fun',
        choices = switch(
          selected_var_type(),
          "numeric"  = math_funs,
          "char"     = char_funs,
          "date"     = date_funs,
          "logical"  = logical_funs,
          "factor"   = factor_funs,
          "complex"  = complex_funs,
          character(0)
        )
      )

    })

    # apply function events ---------------------------------------------------
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
            groupby = input$vars_groupby_fun |> as.list()
          )]

          df$df_active <- copy(temp)
          rm(temp)

          msg('Apply function: OK')

        } else {
          msg_error('New name is not valid or already in use')
        }

      }
    }) |> bindEvent(input$btn_apply_fun)

    # apply freehand events ---------------------------------------------------
    #  groupby vars -----------------------------------------------------------
    output$ui_var_groupby_free <- renderUI({
      selectizeInput(ns('vars_groupby_free'),
                     'Group By',
                     choices = df_names(),
                     multiple = T,
                     options = list(plugins = list('remove_button', 'clear_button'))
                     )
    })

    # calculate var -----------------------------------------------------------
    observe({

      if(is_valid_name(input$new_name_free) &&
         input$new_name_free %notin% df_names()){

        # parse code ----------------------------------------------------------
        parsed_code <- try(parse_expr(input$txt_code_input), silent = T)

        if(inherits(parsed_code, "try-error")){
          return(msg_error('Error to validate expression. Check code'))
        }

        # get operations and variables ----------------------------------------
        code_operations <- all.names(parsed_code, unique = T)
        code_vars <- all.vars(parsed_code)

        code_operations <- code_operations[!code_operations %in% code_vars]

        #  check variables and operations -------------------------------------
        if (!all(code_vars %in% c(df_names(), 'T', 'F'))) {
          msg_error('Some variables are not present in the dataset')
        } else if (!all(code_operations %in% allowed_operations)) {
          msg_error('Some operations are not allowed')
        } else {
          # create safe env foe evaluation ------------------------------------
          e1 <- safe_env(allowed_operations)

          # run code ----------------------------------------------------------
          e1$temp <- copy(df$df_active)

          # start new variable
          e1$new_var_name <- input$new_name_free
          e1$vars_groupby <- input$vars_groupby_free |> as.list()
          e1$parsed_code <- parsed_code

          e1$temp <- eval(expression(

            try(temp[, new_var_name := parsed_code,
                     by = groupby,
                     env = list(new_var_name = new_var_name,
                                parsed_code = parsed_code,
                                groupby = vars_groupby)],
                silent = TRUE)
            ),
            envir = e1
          )

          if(inherits(e1$temp, "try-error")){
            return(msg_error('Error in expression. Check code'))
          } else{

            df$df_active <- copy(e1$temp)
            rm(e1)
          }
        }

      } else {
        msg_error('New name is not valid or already in use')
      }

    }) |> bindEvent(input$btn_apply_free)


    return(list(df_calculate_cols = reactive(df$df_active)))
  })
}
