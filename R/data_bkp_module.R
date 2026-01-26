
# ui --------------------------------------------------------------------------
data_bkp_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    btn_task(ns('btn_reset'), 'Reset Dataset', icon('arrow-rotate-right')) |>
      ttip('Restore to previous state (before been set as the Active dataset)'),
    btn_task(ns('btn_bkp'), 'Create Backup', icon('cloud-arrow-up')) |>
      ttip('Create a copy of the Active dataset'),
    btn_task(ns('btn_restore'), 'Restore Backup', icon('cloud-arrow-down')) |>
      ttip('Restore a previously created backup'),
    btn_task(ns('btn_clear_bkp'), 'Clear Backup', icon('trash-can')) |>
      ttip('Erase the backup'),
  )
}

# server ----------------------------------------------------------------------
data_bkp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  # reset df active ------------------------
	  observe({
	    update_act_dt(session, copy(session$userData$dt$bkp0))
	    msg('Active Dataset Reseted')
	  }) |> bindEvent(input$btn_reset)

	  # create backup --------------------------
	  observe({
	    session$userData$dt$bkp <- copy(get_act_dt(session))
	    msg('Backup created')
	  }) |> bindEvent(input$btn_bkp)

	  # restore backup -------------------------
	  observe({
	    if(is.null(session$userData$dt$bkp)){
	      msg('No backup to restore')
	    } else {
	      update_act_dt(session, copy(session$userData$dt$bkp))
	      msg('Backup restored')
	    }
	  }) |> bindEvent(input$btn_restore)

	  # clear backup ---------------------------
	  observe({
	    if(is.null(session$userData$dt$bkp)){
	      msg('No backup to clear')
	    } else {
	      session$userData$dt$bkp <- NULL
	      msg('Backup cleared')
	    }
	  }) |> bindEvent(input$btn_clear_bkp)

  })
}
