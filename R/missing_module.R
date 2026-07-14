
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
        selectInput(
          ns('sel_replace_method'),
          'Replacement',
          c(
            'Constant'='constant',
            'Mean'='mean',
            'Median'='median'
          )
        ),

        conditionalPanel(
          sprintf("input['%s'] == 'constant'", ns('sel_replace_method')),
          textInput(ns('txt_replace_value'), 'Value')
        )
      ),
      card_footer(btn_task(ns('btn_replace'), 'Replace', icon('wand-magic-sparkles')))
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
	  # update replace methods
	  observe({
	    req(input$var_sel_replace)

	    if (obj_type(get_act_dt(session)[[input$var_sel_replace]]) == 'numeric') {
	      choices <- c(
	        'Constant' = 'constant',
	        'Mean' = 'mean',
	        'Median' = 'median'
	      )
	    } else {
	      choices <- c('Constant' = 'constant')
	    }

	    updateSelectInput(
	      session,
	      "sel_replace_method",
	      choices = choices
	    )
	  }) |> bindEvent(input$var_sel_replace)

    # replace value -----------
	  observe({
	    selected_var <- input$var_sel_replace

	    if(!isTruthy(selected_var)){
	      msg('Select at least one variable')
	      return()
	    }

	    method <- input$sel_replace_method
	    sel_var_type <- obj_type(get_act_dt(session)[[selected_var]])

	    # only allow char and numeric types
	    if(sel_var_type %notin% c('char', 'numeric')){
	      msg('Select a character or numeric variable')
	      return()
	    }

	    # mean and median only for numerics
	    if(method %in% c('mean', 'median') && sel_var_type != 'numeric'){
	      msg('Mean and Median replacement are only available for numeric variables', 4)
	      return()
	    }

	    if(method %in% c('constant') && !isTruthy(input$txt_replace_value)){
	      msg('Inform a value')
	      return()
	    }

	    temp <- copy(get_act_dt(session))
      new_value <- switch(
        method,
        'mean' = mean_nona(temp[[selected_var]]),
        'median' = median_nona(temp[[selected_var]]),
        'constant' = input$txt_replace_value
      )

      if(sel_var_type == 'numeric'){
        new_value <- as.numeric(new_value)
      } else {
        new_value <- as.character(new_value)
      }

      temp <- temp[, var := fcoalesce(var, new_value), env = list(var = selected_var)]

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
