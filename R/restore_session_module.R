
# ui --------------------------------------------------------------------------
restore_session_ui <- function(id) {
  ns <- NS(id)

}

# server ----------------------------------------------------------------------
restore_session_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  # restore saved session data ----------------------------------------------
	  observe({
	    req(session$userData$conf$data_dir,
	        session$userData$conf$restore_session,
	        session$userData$conf$restore_data_status,
	        session$userData$conf$restore_output_status,
	        session$userData$dt$dt
	    )

	    if(session$userData$conf$restore_session == 'always') {

	      if(!file.exists(paste0(session$userData$conf$data_dir, '/data.qs2'))){
	        session$userData$conf$restore_data_status <- 2

	      } else {

	        previous_data <- qs_read(paste0(session$userData$conf$data_dir, '/data.qs2'))

	        # check data format
	        if(!test_data_format(previous_data)){
	          session$userData$conf$restore_data_status <- 3
	        } else {

	          previous_data <- lapply(previous_data, as.data.table)

	          # if empty entry only keep loaded data
	          if(session$userData$conf$empty_datasets == 1){
	            session$userData$dt$dt <- previous_data
	            # update meta
	            session$userData$dt$meta <- lapply(previous_data, df_info)
	          } else {
	            previous_data <- make_names_append_list(
	              previous_data,
	              names(session$userData$dt$dt)
	            )

	            session$userData$dt$dt <- c(previous_data, session$userData$dt$dt)

	            # append meta
	            new_meta <- lapply(previous_data, df_info)

	            session$userData$dt$meta <- c(
	              new_meta,
	              session$userData$dt$meta
	            )
	          }

	          session$userData$dt$act_name <- names(session$userData$dt$dt)[1]
	          session$userData$conf$restore_data_status <- 1
	        }
	      }

	      # import output
	      if(!file.exists(paste0(session$userData$conf$data_dir, '/output.qs2'))){
	        session$userData$conf$restore_output_status <- 2

	      } else {
	        previous_output <- qs_read(paste0(session$userData$conf$data_dir, '/output.qs2'))

	        # check output format
	        if(!test_output_format(previous_output)){
	          session$userData$conf$restore_output_status <- 3
	        } else {
	          previous_output <- lapply(previous_output, function(x) {

	            id <- gen_element_id()
	            x$id <- id
	            btn_id <- paste0("btn_xout_", id)

	            x$btn <- actionButton(
	              ns(btn_id),
	              '',
	              icon('x'),
	              class = 'micro-btn-cancel'
	            )

	            observe({
	              session$userData$out$elements[[id]] <- NULL
	            }) |> bindEvent(input[[btn_id]], once = TRUE)

	            return(x)
	          })
            # rename items of list with new ids
	          names(previous_output) <- vapply(previous_output, function(x) x$id, character(1))

	          session$userData$out$elements <- previous_output
	          session$userData$conf$restore_output_status <- 1
	        }
	      }
	    }

	    session$userData$conf$restore_status <- paste0(
	      session$userData$conf$restore_data_status, '.',
	      session$userData$conf$restore_output_status
	    )

	  }) |> bindEvent(session$userData$conf$restore_session, once = T)

	  # show modal with restored status
	  observe({
	    req(session$userData$conf$restore_status)

	    if(any(session$userData$conf$restore_session %in% c('always', 'ask'))){

	      display_restore_status(
	        session$userData$conf$restore_status,
	        actionButton(ns('btn_dismiss_restore_status'), 'OK', class = 'btn-task')
	      )

	    }
	  }) |> bindEvent(session$userData$conf$restore_status, once = T)

	  observe({
	    removeModal()

	  }) |> bindEvent(input$btn_dismiss_restore_status)

  })
}
