
# ui --------------------------------------------------------------------------
missing_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Identify Missing Values', class = 'mini-header'),
      card_body(
        selectInput(ns('var_sel_missing'), 'Variable', ''),
        textInput(ns('txt_missing_name'), 'Missing Variable Name', 'Missing')
      ),
      card_footer(btn_task(ns('btn_identify'), 'Run', icon('check')))
    ),
    card(
      card_header('Replace Missing Values', class = 'mini-header'),
      card_body(
        selectInput(ns('var_sel_replace'), 'Variable', ''),
        uiOutput(ns('ui_replace_method')),
        uiOutput(ns('ui_replace_value'))
      ),
      card_footer(btn_task(ns('btn_replace'), 'Replace', icon('check')))
    )
  )
}

# server ----------------------------------------------------------------------
missing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  df_names <- reactive(get_act_dt(session) |> names())

	  # update inputs ------------
	  observe({
	    req(df_names())
	    updateSelectInput(
	      session,
	      'var_sel_missing',
	      choices = c('', df_names())
	    )

	    updateSelectInput(
	      session,
	      'var_sel_replace',
	      choices = c('', df_names())
	    )
	  }) |> bindEvent(df_names())

	  # detect missing -----------
	  observe({

	    selected_var <- input$var_sel_missing
	    missing_name <- input$txt_missing_name

	    if(!isTruthy(selected_var)){
	      msg('Select at least one variable')
	      return()
	    } else {
	      if(is_name_available(missing_name, df_names())){

  	      running_modal()
  	      temp <- copy(get_act_dt(session))

  	      temp[, ]

  	      temp[, new_var := factor(is.na(sel_var), levels = c(TRUE, FALSE), labels = c('Missing', 'Not Missing' )), env = list(
  	        new_var = missing_name,
  	        sel_var = selected_var
  	      )]

  	      update_act_dt(session, copy(temp), updated_cols = missing_name)
  	      rm(temp)

  	      remove_running_modal()

  	      updateTextInput(session, 'txt_missing_name', value = 'Missing')

  	    } else {
  	      msg_error('New names are not valid or already in use')
  	      return()
  	    }
	    }
	  }) |> bindEvent(input$btn_identify)

	  # replace missing -----------
	  output$ui_replace_method <- renderUI({
	    req(input$var_sel_replace)
	    type <- obj_type(get_act_dt(session)[[input$var_sel_replace]])

	    methods <- c('Constant' = 'constant')

	    if (type %in% c('double', 'integer')) {
	      methods <- c(methods, 'Mean' = 'mean', 'Median' = 'median')
	    }

	    selectInput(
	      ns('sel_replace_method'),
	      'Replacement',
	      choices = methods
	    )
	  })

	  output$ui_replace_value <- renderUI({
	    req(input$var_sel_replace, input$sel_replace_method)

	    type <- obj_type(get_act_dt(session)[[input$var_sel_replace]])

      if(type %in% c('double', 'integer')) {

        if (input$sel_replace_method != 'constant') {
          NULL
        } else {
          numericInput(ns('num_value'), 'Value', 0)
        }
      } else if(type == 'logical') {
        selectInput(ns('logical_value'), 'Value', c(TRUE, FALSE))
      } else if(type == 'factor') {
        selectInput(
          ns('factor_value'),
          'Value',
          choices = levels(get_act_dt(session)[[input$var_sel_replace]])
        )
      } else if(type %in% c('date', 'posix')) {
        dateInput(ns('date_value'), 'Date')
      } else {
        textInput(ns('txt_replace_value'), 'Value')
      }
	  })

    # replace value -----------
	  observe({
	    selected_var <- input$var_sel_replace

	    if(!isTruthy(selected_var)){
	      msg('Select at least one variable')
	      return()
	    }

	    method <- input$sel_replace_method
	    sel_var_type <- obj_type(get_act_dt(session)[[selected_var]])

	    replace_value <- switch(
	      sel_var_type,
	      'double' = input$num_value |> as.double(),
	      'integer' = input$num_value |> as.integer(),
	      'char' = input$txt_replace_value,
	      'logical' = input$logical_value |> as.character(),
	      'factor' = input$factor_value |> as.factor(),
	      'date' = input$date_value |> as.Date(),
	      'posix' = input$date_value |> as.Date()
	    )

	    if(method %in% c('constant') && !isTruthy(replace_value)){
	      msg('Inform a value')
	      return()
	    }

	    temp <- copy(get_act_dt(session))

      new_value <- switch(
        method,
        'mean' = mean_nona(temp[[selected_var]]),
        'median' = median_nona(temp[[selected_var]]),
        'constant' = replace_value
      )

      # new value must keep the type of actual variable
      actual_type <- temp[[selected_var]] |> obj_type()
      new_value <- convert(new_value, actual_type)

      temp <- try(
        temp[is.na(var), var := new_value, env = list(var = selected_var)],
        silent = TRUE
      )

      if (inherits(temp, 'try-error')) {
        msg_error('Unable to replace missing values')
        return()
      }

      if(!is_spada_df(temp)){
        abort_filter_modal()
      } else {
        running_modal()
        update_act_dt(session, copy(temp))

        updateSelectInput(session, 'var_sel_replace', selected = character(0))
        remove_running_modal()
      }
	  }) |> bindEvent(input$btn_replace)

  })
}
