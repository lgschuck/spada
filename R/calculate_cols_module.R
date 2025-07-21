
# ui --------------------------------------------------------------------------
calculate_cols_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Apply Function', class = 'mini-header'),
      card_body(
        selectInput(ns('vars_sel'), 'Variable', NULL),

        selectInput(ns('fun'), 'Choose a function', character(0)),
        textInput(ns('txt_new_name_fun'), 'New variable name'),
        selectizeInput(ns('vars_groupby_fun'), 'Group by', NULL,
                       multiple = T,
                       options = list(plugins = list('remove_button', 'clear_button')))

      ),
      card_footer(btn_task(
        ns('btn_apply_fun'), 'Apply Function', icon('check')
      ))
    ),
    card(
      card_header('Freehand', class = 'mini-header'),
      card_body(
        textInput(ns('txt_new_name_free'), 'New var name', placeholder = 'new_var'),
        textAreaInput(ns('txt_code_input'), 'Input code', width = '800px', height = '200px'),
        column(4, btn_task(ns('btn_allowed_operations'), 'Show Allowed Operations')),
        selectizeInput(ns('vars_groupby_free'),
                       'Group By',
                       choices = NULL,
                       multiple = T,
                       options = list(plugins = list('remove_button', 'clear_button'))
        )
      ),
      card_footer(btn_task(
        ns('btn_apply_free'), 'Run Code', icon('gears')
      ))
    )
  )
}

# server ----------------------------------------------------------------------
calculate_cols_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  df_names <- reactive(get_act_dt(session) |> names())

	  observe({
	    req(df_names())
	    updateSelectInput(
	      session,
	      'vars_sel',
	      choices = c('', df_names())
	    )
	  }) |> bindEvent(df_names())

    # render variables group -------------------------------------------------
	  observe({
	    req(df_names())
	    updateSelectizeInput(
	      session,
	      'vars_groupby_fun',
	      choices = c('', df_names())
	    )
	  }) |> bindEvent(df_names())


    # suggest name for new variable -------------------------------------------
    observe({
      req(input$vars_sel, input$fun)
      updateTextInput(session, 'txt_new_name_fun',
                      value = paste0(input$vars_sel, '_', input$fun))
    }) |> bindEvent(input$vars_sel, input$fun)

    # render functions choices ------------------------------------------------
    selected_var_type <- reactive({
      req(input$vars_sel)
      obj_type(get_act_dt(session)[[input$vars_sel]])
    })

    observe({
      req(selected_var_type())

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
    }) |> bindEvent(selected_var_type())

    # apply function events ---------------------------------------------------
    observe({
      if(!isTruthy(input$vars_sel)) {
        msg('Select at least one variable')
      } else if(!isTruthy(input$fun)){
        msg('Select a function')
      } else {
        if(is_valid_name(input$txt_new_name_fun) &&
            input$txt_new_name_fun %notin% df_names()) {

          temp <- copy(get_act_dt(session))

          temp[, new_var := fun(var1), by = groupby, env = list(
            new_var = input$txt_new_name_fun,
            fun = input$fun,
            var1 = input$vars_sel,
            groupby = input$vars_groupby_fun |> as.list()
          )]

          update_act_dt(session, copy(temp))
          rm(temp)

          msg('Apply function: OK')

          updateTextInput(session, 'txt_new_name_fun', value = '')

        } else {
          msg_error('New name is not valid or already in use')
        }

      }
    }) |> bindEvent(input$btn_apply_fun)

    # apply freehand events ---------------------------------------------------
    #  groupby vars -----------------------------------------------------------
    observe({
      req(df_names())
      updateSelectizeInput(
        session,
        'vars_groupby_free',
        choices = c('', df_names())
      )
    }) |> bindEvent(df_names())

    # calculate var -----------------------------------------------------------
    observe({

      if(is_valid_name(input$txt_new_name_free) &&
         input$txt_new_name_free %notin% df_names()){

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
          e1$temp <- copy(get_act_dt(session))

          # start new variable
          e1$new_var_name <- input$txt_new_name_free
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

            update_act_dt(session, copy(e1$temp))

            msg('Calculate new var: OK')
            rm(e1)

            updateTextInput(session, 'txt_new_name_free',
                            value = '', placeholder = 'new_var')

            updateTextAreaInput(session, 'txt_code_input', value = '')

          }
        }

      } else {
        msg_error('New name is not valid or already in use')
      }

    }) |> bindEvent(input$btn_apply_free)

    # show allowed operations -------------------------------------------------
    observe({
      show_allowed_op()
    }) |> bindEvent(input$btn_allowed_operations)

  })
}
