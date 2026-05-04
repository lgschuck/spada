
# ui --------------------------------------------------------------------------
duplicates_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Identify Duplicates', class = 'mini-header'),
      card_body(
        selectizeInput(
          ns('vars_sel_dup'),
          'Variables',
          '',
          multiple = T,
          options = list(plugins = list('remove_button', 'clear_button')),
          width = '80%'
        ),
        textInput(ns('txt_new_dup_name'), 'Duplicaeted Variable Name', 'Duplicated'),
        radioButtons(ns('radio_last'), 'Which Duplicated',
                     c('First case' = TRUE, 'Last case' = FALSE), inline = T),
        radioButtons(ns('radio_primary'), 'Mark Primary Cases',
                     c('Yes' = TRUE, 'No' = FALSE), inline = T),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'TRUE'", ns('radio_primary')),
          textInput(ns('txt_new_primary_name'), 'Primary Variable Name', 'Primary')
        )
      ),
      card_footer(btn_task(ns('btn_identify'), 'Run', icon('check')))
    ),
    card(
      card_header('Remove Duplicates', class = 'mini-header'),
      card_body(
        selectInput(ns('vars_sel_drop'), 'Variable', NULL),
      ),
      card_footer(btn_task(ns('btn_remove'), 'Remove', icon('trash-can')))
    )
  )
}

# server ----------------------------------------------------------------------
duplicates_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  df_names <- reactive(get_act_dt(session) |> names())

	  # update inputs ------------
	  observe({
	    req(df_names())
	    updateSelectizeInput(
	      session,
	      'vars_sel_dup',
	      choices = c('', df_names())
	    )

	    updateSelectizeInput(
	      session,
	      'vars_sel_drop',
	      choices = c('', df_names())
	    )
	  }) |> bindEvent(df_names())

	  # detect duplicates -----------
	  observe({

	    selected_vars <- input$vars_sel_dup
	    dup_name <- input$txt_new_dup_name
	    last_dup <- as.logical(input$radio_last)
	    primary <- as.logical(input$radio_primary)
	    primary_name <- input$txt_new_primary_name

	    new_vars <- if(primary){
	      c(dup_name, primary_name)
	    } else {
	      dup_name
	    }

	    if(selected_vars |> length() == 0){
	      msg('Select at least one variable')
	      return()
	    } else {
	      if(is_name_available(dup_name, df_names()) &&
	         (!primary || is_name_available(primary_name, df_names()))
	       ){

  	      running_modal()
  	      temp <- copy(get_act_dt(session))

  	      dup_vector <- duplicated(temp, fromLast = last_dup, by = selected_vars)

  	      dup_factor <- factor(
  	        dup_vector,
  	        levels = c(TRUE, FALSE),
  	        labels = c('Duplicated', 'Not Duplicated')
  	      )

  	      temp[, new_var := dup, env = list(
  	        new_var = dup_name,
  	        dup = dup_factor
  	      )]

  	      if(primary){
  	        primary_factor <- factor(
  	          !dup_vector,
  	          levels = c(TRUE, FALSE),
  	          labels = c('Primary', 'Not Primary')
  	        )

  	        temp[, new_var := p, env = list(
  	          new_var = primary_name,
  	          p = primary_factor
  	        )]
  	      }

  	      update_act_dt(session, copy(temp), updated_cols = new_vars)
  	      rm(temp)

  	      remove_running_modal()

  	      updateTextInput(session, 'txt_new_dup_name', value = 'Duplicated')
  	      updateTextInput(session, 'txt_new_primary_name', value = 'Primary')

  	    } else {
  	      msg_error('New names are not valid or already in use')
  	      return()
  	    }
	    }
	  }) |> bindEvent(input$btn_identify)

	  # drop duplicates -----------
	  observe({

	    selected_vars <- input$vars_sel_drop

	    not_dup_factor <- !(is.factor(get_act_dt(session)[[selected_vars]]) &&
	      all(levels(get_act_dt(session)[[selected_vars]]) %in% c('Duplicated', 'Not Duplicated')))

	    if(selected_vars |> length() == 0){
	      msg('Select at least one variable')
	      return()
	    } else if(not_dup_factor){
	      msg('The variable is not a factor of Duplicated and Not Duplicated Levels', 4)
	      return()
	    } else {

        temp <- copy(get_act_dt(session))
        temp <- temp[var == 'Not Duplicated', , env = list(var = selected_vars)]

        if(!is_spada_df(temp)){
          abort_filter_modal()
        } else {
          running_modal()
          update_act_dt(session, copy(temp))
          remove_running_modal()
        }
      }
	  }) |> bindEvent(input$btn_remove)

  })
}
