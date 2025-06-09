
# Function with the server of spada.R
spada_server <- function(datasets){
  function(input, output, session) {

    options(shiny.maxRequestSize = 1000 * 1024 ^ 2)

    # data --------------------------------------------------------------------
    session$userData$dt <- reactiveValues(
      dt = lapply(
        datasets,
        \(df) {
          df_temp <- lapply(df, make_valid_cols) |> as.data.frame()
          df_temp |> setDT()
        }
      )
    )

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    # start values
    session$userData$df <- reactiveValues(
      act = lapply(datasets[[1]], make_valid_cols) |> setDT(),
      act_name = names(datasets[1]),
      bkp = NULL
    )

    output$df_active_name <- renderText(
      if(nchar(session$userData$df$act_name) <= 20){
        session$userData$df$act_name
      } else {
        paste0(substr(session$userData$df$act_name, 1, 17) , '...')
      }
    )

    # navbar ------------------------------------------------------------------
    navbar_df_info_server('navbar_df_info', app_session = session)

    # side bar ----------------------------------------------------------------
    sidebar_server('sidebar', app_session = session)

    # data page events --------------------------------------------------------
    session$userData$df$act_meta <- reactive({ df_info(session$userData$df$act) })

    output$pD_metadata_gt <- render_gt(session$userData$df$act_meta() |> gt_info())

    # overview -------------------------------
    data_overview_server('pD_overview')

    # values for boxes -----------------------
    data_highlights_server('pD_highlights')

    # define active dataset ---------------------------------------------------
    output$pD_data_ui_sel_df <- renderUI({
      req(session$userData$dt_names())
       selectInput('pD_data_sel_df', 'Select a dataset', session$userData$dt_names())
    })

    # make active dataset event --------------
    observe({
      # save active to original data
      session$userData$dt$dt[[session$userData$df$act_name]] <- session$userData$df$act
      # choose new dataset to be active
      session$userData$df$act <- session$userData$dt$dt[[input$pD_data_sel_df]]
      session$userData$df$bkp <- NULL

      session$userData$df$act_name <- input$pD_data_sel_df

      msg(paste('Dataset', session$userData$df$act_name, 'is the active one'))
      gc()
      updateTextInput(session, "pD_data_txt_new_name", value = '')
    }) |> bindEvent(input$pD_data_btn_active)

    # rename event ---------------------------
    observe({
      if(!is_valid_name(input$pD_data_txt_new_name) |
         input$pD_data_txt_new_name %in% session$userData$dt_names()){
        msg_error('New name is not valid or already in use')
      } else {
        names(session$userData$dt$dt)[session$userData$dt_names() == input$pD_data_sel_df] <- input$pD_data_txt_new_name
        # update active dataset if necessary
        if(session$userData$df$act_name == input$pD_data_sel_df){
          session$userData$df$act <- session$userData$dt$dt[[input$pD_data_txt_new_name]]

          session$userData$df$act_name <- input$pD_data_txt_new_name
        }
        msg('New name applied')
        updateTextInput(session, "pD_data_txt_new_name", value = '')
      }
    }) |> bindEvent(input$pD_data_btn_new_name)

    # copy dataset event ---------------------
    observe({
      if(!is_valid_name(input$pD_data_txt_new_name) ||
         (input$pD_data_txt_new_name %in% session$userData$dt_names())){
        msg_error('New name is not valid or already in use')
      } else {
        session$userData$dt$dt[[ input$pD_data_txt_new_name ]] <- session$userData$df$act
        msg(paste('Dataset', input$pD_data_txt_new_name, 'created'))
        gc()
        updateTextInput(session, "pD_data_txt_new_name", value = '')
      }
    }) |> bindEvent(input$pD_data_btn_copy_dataset)

    # delete dataset event -------------------
    observe({
      if(session$userData$df$act_name == input$pD_data_sel_df){
        msg_error('You can not delete the active dataset')
      } else {
        session$userData$dt$dt[[ input$pD_data_sel_df ]] <- NULL
        msg(paste('Dataset', input$pD_data_sel_df, 'deleted'))
        gc()
        updateTextInput(session, "pD_data_txt_new_name", value = '')
      }
    }) |> bindEvent(input$pD_data_btn_delete_dataset)

    # export file -------------------------------------------------------------
    export_file_server('pD_export')

    # import file -------------------------------------------------------------
    import_file_server('pD_import')

    # edit page events --------------------------------------------------------

    # filter events --------------------------
    filter_rows_server('pE_filter_rows')

    # select cols ----------------------------
    select_cols_server('pE_filter_sel_cols')

    # convert events -------------------------
    convert_cols_server('pE_convert_cols')

    # order rows events ----------------------
    order_rows_server('pE_order_rows')

    # order cols events ----------------------
    order_cols_server('pE_order_cols')

    # rename cols events ---------------------
    rename_cols_server('pE_rename_cols')

    # calculate cols events ------------------
    calculate_cols_server('pE_calculate_cols')

    # reset df active ------------------------
    observe({
      session$userData$df$act <- copy(session$userData$dt$dt[[session$userData$df$act_name]])
      msg('Active Dataset Reseted')
    }) |> bindEvent(input$pE_btn_reset)

    # create backup --------------------------
    observe({
      session$userData$df$bkp <- copy(session$userData$df$act)
      msg('Backup created')
    }) |> bindEvent(input$pE_btn_bkp)

    # restore backup -------------------------
    observe({
      if(is.null(session$userData$df$bkp)){
        msg('No backup to restore')
      } else {
        session$userData$df$act <- copy(session$userData$df$bkp)
        msg('Backup restored')
      }
    }) |> bindEvent(input$pE_btn_restore)

    # clear backup ---------------------------
    observe({
      if(is.null(session$userData$df$bkp)){
        msg('No backup to clear')
      } else {
        session$userData$df$bkp <- NULL
        msg('Backup cleared')
      }
    }) |> bindEvent(input$pE_btn_clear_bkp)

    # analysis page events ----------------------------------------------------
    exploratory_server('pA_exploratory')

    # descriptive stats -------------------------------------------------------
    descriptive_stats_server('pA_desc_stats')

    # correlation -------------------------------------------------------------
    correlation_server('pA_correlation')

    # normality test ----------------------------------------------------------
    normality_test_server('pA_normality_test')

    # z test ------------------------------------------------------------------
    z_test_server('pA_z_test')

    # linear model ------------------------------------------------------------
    lm_server('pA_lm')

    # output events -----------------------------------------------------------
    session$userData$out <- reactiveValues(elements = list(report_card()))

    output_server('pO_output')

    # config events -----------------------------------------------------------
    mod_pC <- page_config_server('pC')

    # about events ------------------------------------------------------------
    about_spada_server('about_spada')

    # exit app event ----------------------------------------------------------
    session$onSessionEnded(stopApp)
    observe({
      req(input$navbar)
      if (input$navbar == 'exit') {
        session$sendCustomMessage(type = 'closeWindow', message = 'message')
        stopApp()
      }
    })
  }
}
