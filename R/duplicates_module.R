
# ui --------------------------------------------------------------------------
duplicates_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    card(
      card_header('Identify Duplicates', class = 'mini-header'),
      card_body(
        selectizeInput(
          ns('vars_sel'),
          'Variable',
          NULL,
          multiple = T,
          options = list(plugins = list('remove_button', 'clear_button')),
          width = '80%'
        ),
        textInput(ns('txt_new_dup_name'), 'Duplicated var name', 'Duplicated'),
        radioButtons(ns('radio_last'), 'Which Duplicated',
                     c('First' = TRUE, 'Last' = FALSE), inline = T),
        radioButtons(ns('radio_primary'), 'Mark Primary Cases',
                     c('Yes' = TRUE, 'No' = FALSE), inline = T),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'TRUE'", ns('radio_primary')),
          textInput(ns('txt_new_primary_name'), 'Primary var name', 'Primary')
        )
      ),
      card_footer(btn_task(ns('btn_apply'), 'Apply', icon('check')))
    ),
    card(

    )
  )
}

# server ----------------------------------------------------------------------
duplicates_server <- function(id) {
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

	    selected_vars <- input$vars_sel
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
	         is_name_available(primary_name, df_names())
	       ){

  	      running_modal()
  	      temp <- copy(get_act_dt(session))

  	      dup_vector <- duplicated(temp, fromLast = last_dup, by = selected_vars)

  	      temp[, new_var := dup, env = list(
  	        new_var = dup_name,
  	        dup = as.integer(dup_vector)
  	      )]

  	      if(primary){
  	        temp[, new_var := p, env = list(
  	          new_var = primary_name,
  	          p = as.integer(!dup_vector)
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
	  }) |> bindEvent(input$btn_apply)

  })
}
