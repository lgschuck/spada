
# Function with the server of spada.R
spada_server <- function(datasets, conf){
  function(input, output, session) {

    options(shiny.maxRequestSize = conf$file_size * 1024 ^ 2)

    # conf values -------------------------------------------------------------
    session$userData$conf <- reactiveValues(
      empty_datasets = conf$empty_datasets,
      conf_dir = conf$conf_dir,
      data_dir = conf$data_dir,
      theme = conf$theme,
      file_size = conf$file_size,
      restore_session = conf$restore_session,
      save_session = conf$save_session,
      restore_data_status = conf$restore_data_status,
      restore_output_status = conf$restore_output_status,
      restore_status = conf$restore_status,
      plot_fill_color = conf$plot_fill_color,
      plot_line_color = conf$plot_line_color
    )

    # restore saved session data -----------------------------------------------
    observe({
      req(session$userData$conf$data_dir,
          session$userData$conf$restore_session,
          session$userData$conf$restore_data_status,
          session$userData$conf$restore_output_status,
          session$userData$dt$dt,
          session$userData$df$act
          )

      if(session$userData$conf$restore_session == 'always') {

        if(!file.exists(paste0(session$userData$conf$data_dir, '/data.RDS'))){
          session$userData$conf$restore_data_status <- 2

        } else {

          previous_data <- readRDS(
            paste0(session$userData$conf$data_dir, '/data.RDS'))

          # check data format
          if(!test_data_format(previous_data)){
            session$userData$conf$restore_data_status <- 3
          } else {

            previous_data <- lapply(previous_data, as.data.table)

            # if empty entry only keep loaded data
            if(session$userData$conf$empty_datasets == 1){
              session$userData$dt$dt <- previous_data
            } else {
              previous_data <- make_names_append_list(
                previous_data,
                names(session$userData$dt$dt)
              )
              session$userData$dt$dt <- append(previous_data, session$userData$dt$dt)
            }

            session$userData$df$act <- session$userData$dt$dt[[1]]
            session$userData$df$act_name <- names(session$userData$dt$dt[1])

            session$userData$conf$restore_data_status <- 1
          }
        }

        # import output
        if(!file.exists(paste0(session$userData$conf$data_dir, '/output.RDS'))){
          session$userData$conf$restore_output_status <- 2

        } else {
          previous_output <- readRDS(paste0(session$userData$conf$data_dir, '/output.RDS'))

          # check output format
          if(!test_output_format(previous_output)){
            session$userData$conf$restore_output_status <- 3
          } else {
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

        list_check_restore <-
          tags$span(
            switch(
              substr(session$userData$conf$restore_status, 1, 1),
              '1' = h5(tagList(icon('check', style = 'color: green; margin-right: 5px;'), 'Data restored')),
              '2' = h5(tagList(icon('times', style = 'color: red; margin-right: 5px;'), 'Data not found')),
              '3' = h5(tagList(icon('circle-question', style = 'color: red; margin-right: 5px;'), 'Data in invalid format'))
            ),
            tags$br(),
            switch(
              substr(session$userData$conf$restore_status, 3, 3),
              '1' = h5(tagList(icon('check', style = 'color: green; margin-right: 5px;'), 'Output restored')),
              '2' = h5(tagList(icon('times', style = 'color: red; margin-right: 5px;'), 'Output not found')),
              '3' = h5(tagList(icon('circle-question', style = 'color: red; margin-right: 5px;'), 'Output in invalid format'))
            )
          )

        showModal(modalDialog(
          title = 'Session Restore Status',
          list_check_restore,
          easyClose = FALSE,
          size = 'l',
          footer = tagList(
            actionButton('btn_dismiss_restore_sesison', 'OK', class = 'btn-task')
          )
        ))
      }
    }) |> bindEvent(session$userData$conf$restore_status, once = T)

    observe({
      removeModal()
    }) |> bindEvent(input$btn_dismiss_restore_sesison)

    # data --------------------------------------------------------------------
    session$userData$dt <- reactiveValues(
      dt = lapply(
        datasets,
        \(df) {
          df_temp <- lapply(df, make_valid_cols) |> as.data.frame()
          df_temp |> as.data.table()
        }
      )
    )

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    # # start values
    session$userData$df <- reactiveValues(
      act = lapply(datasets[[1]], make_valid_cols) |> as.data.table(),
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

    # datasets metadata -------------------------------------------------------
    session$userData$dt$df_info <- reactive({
      req(session$userData$df$act, session$userData$df$act_name, session$userData$dt$dt)

      datasets <- c(
        list(session$userData$df$act),
        session$userData$dt$dt[names(session$userData$dt$dt) != session$userData$df$act_name]
      )

      names(datasets)[1] <- session$userData$df$act_name

      lapply(datasets, df_info)
    })

    session$userData$dt$gt_info <- reactive({
      req(session$userData$dt$df_info)

      Map(gt_info, session$userData$dt$df_info(),
          df_name = names(session$userData$dt$df_info()))
    })

    # df active metadata ---------------------
    session$userData$df$act_meta <- reactive({
      req(session$userData$dt$df_info(), session$userData$df$act_name)
      session$userData$dt$df_info()[[session$userData$df$act_name]]
    })

    # navbar ------------------------------------------------------------------
    navbar_df_info_server('navbar_df_info', app_session = session)

    # side bar ----------------------------------------------------------------
    sidebar_server('sidebar', app_session = session)

    # metadata ----------------------------------------------------------------
    metadata_server('pD_metadata')

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

    # rename dataset event -------------------
    observe({
      if(!is_valid_name(input$pD_data_txt_new_name) |
         input$pD_data_txt_new_name %in% session$userData$dt_names()){
        msg_error('New name is not valid or already in use')
      } else {
        # save active to original data
        session$userData$dt$dt[[session$userData$df$act_name]] <- session$userData$df$act
        names(session$userData$dt$dt)[session$userData$dt_names() == input$pD_data_sel_df] <- input$pD_data_txt_new_name
        # update active dataset if necessary
        if(session$userData$df$act_name == input$pD_data_sel_df){
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
        # save active to original data
        session$userData$dt$dt[[session$userData$df$act_name]] <- session$userData$df$act

        session$userData$dt$dt[[input$pD_data_txt_new_name]] <- session$userData$dt$dt[[input$pD_data_sel_df]]
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
        session$userData$dt$dt[[input$pD_data_sel_df]] <- NULL
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
    mod_pC <- config_server('pC')

    # about events ------------------------------------------------------------
    about_spada_server('about_spada')

    # exit app event ----------------------------------------------------------
    session$onSessionEnded(stopApp)
    observe({
      req(input$navbar)
      if (input$navbar == 'exit') {

        if(session$userData$conf$save_session == 'ask'){
          # ask to save data and ouput
          showModal(modalDialog(
            title = 'Save Session',
            'Do you want to save Data and Output objects?',
            easyClose = FALSE,
            size = 'm',
            footer = tagList(
              actionButton('btn_cancel_exit', 'Cancel', icon = icon('xmark'), class = 'btn-task btn-task-cancel'),
              actionButton('btn_cancel_save_session', 'No', icon = icon('xmark'), class = 'btn-task'),
              actionButton('btn_confirm_save_session', 'Yes', icon = icon('check'), class = 'btn-task')
            )
          ))
        } else if (session$userData$conf$save_session == 'never'){
          exit_without_save(session)
        } else if (session$userData$conf$save_session == 'always'){
          exit_with_save(session)
        }
      }
    })

    # cancel exit ----------------------------
    observe({
      removeModal()
      nav_select('navbar', selected = 'Data', session = session)
      nav_select('navset_card_pill_data', selected = 'Highlights', session = session)
    }) |> bindEvent(input$btn_cancel_exit)

    # cancel save session --------------------
    observe({
      removeModal()
      exit_without_save(session)
    }) |> bindEvent(input$btn_cancel_save_session)

    # confirm save session -------------------
    observe({
      removeModal()
      exit_with_save(session)
    }) |> bindEvent(input$btn_confirm_save_session)

  }
}
