
# ui --------------------------------------------------------------------------
data_ui <- function(id) {
  ns <- NS(id)

  card(
    card_body(
      uiOutput(ns('ui_sel_dt')),
      textInput(ns('txt_new_name'), 'New name'),
      layout_column_wrap(
        btn_task(ns('btn_new_name'), 'Rename dataset', icon('file-signature')),
        btn_task(ns('btn_active'), 'Make dataset Active', icon('check')),
        btn_task(ns('btn_copy_dt'), 'Copy dataset', icon('copy')),
        btn_task(ns('btn_del_dt'), 'Delete dataset', icon('trash-can'))
      )
    )
  )
}

# server ----------------------------------------------------------------------
data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  # define active dataset ---------------------------------------------------
	  output$ui_sel_dt <- renderUI({
	    req(session$userData$dt_names())
	    selectInput(
	      ns('sel_dt'),
	      'Select a dataset',
	      choices = c(
	        session$userData$dt$act_name,
	        setdiff(session$userData$dt_names(), session$userData$dt$act_name)
	      )
	    )
	  })

	  # make active dataset event --------------
	  observe({
	    req(input$sel_dt)
	    session$userData$dt$act_name <- input$sel_dt
	    session$userData$dt$bkp0 <- copy(get_act_dt(session))
	    session$userData$dt$bkp <- NULL
	    msg(paste('Dataset', session$userData$dt$act_name, 'is the active one'))
	    updateTextInput(session, 'txt_new_name', value = '')
	  }) |> bindEvent(input$btn_active)

	  # rename dataset event -------------------
	  observe({
	    if(!is_valid_name(input$txt_new_name) |
	       input$txt_new_name %in% session$userData$dt_names()){
	      msg_error('New name is not valid or already in use')
	    } else {
	      names(session$userData$dt$dt)[session$userData$dt_names() == input$sel_dt] <- input$txt_new_name
	      # update active dataset if necessary
	      if(session$userData$dt$act_name == input$sel_dt){
	        session$userData$dt$act_name <- input$txt_new_name
	        # update metadata names
	        names(session$userData$dt$meta)[names(session$userData$dt$meta) == input$sel_dt] <- input$txt_new_name
	      }

	      msg('New name applied')
	      updateTextInput(session, 'txt_new_name', value = '')
	    }
	  }) |> bindEvent(input$btn_new_name)

	  # copy dataset event ---------------------
	  observe({
	    if(!is_valid_name(input$txt_new_name) ||
	       (input$txt_new_name %in% session$userData$dt_names())){
	      msg_error('New name is not valid or already in use')
	    } else {
	      session$userData$dt$dt[[input$txt_new_name]] <- session$userData$dt$dt[[input$sel_dt]]
	      # update metadata
	      session$userData$dt$meta[[input$txt_new_name]] <- session$userData$dt$meta[[input$sel_dt]]
	      gc()

	      msg(paste('Dataset', input$txt_new_name, 'created'))

	      updateTextInput(session, 'txt_new_name', value = '')
	    }
	  }) |> bindEvent(input$btn_copy_dt)

	  # delete dataset event -------------------
	  observe({
	    if(session$userData$dt$act_name == input$sel_dt){
	      msg_error('You can not delete the active dataset')
	    } else {
	      session$userData$dt$dt[[input$sel_dt]] <- NULL

	      # delete metadata
	      session$userData$dt$meta[[input$sel_dt]] <- NULL
	      gc()
	      msg(paste('Dataset', input$sel_dt, 'deleted'))

	      updateTextInput(session, 'txt_new_name', value = '')
	    }
	  }) |> bindEvent(input$btn_del_dt)

  })
}
