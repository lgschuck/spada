
# Function with the server of spada.R
spada_server <- function(datasets, conf){
  function(input, output, session) {

    session$onFlushed(function() {
      waiter_hide()
    })

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
      plot_line_color = conf$plot_line_color,
      plot_title_color = conf$plot_title_color,
      plot_limit = conf$plot_limit
    )

    # restore saved session data -----------------------------------------------
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

        list_check_restore <- div(
          style = 'display:flex; flex-direction:column; gap:12px;',

          switch(
            substr(session$userData$conf$restore_status, 1, 1),
            '1' = status_row('check', '#2e7d32', 'Data restored successfully'),
            '2' = status_row('times', '#c62828', 'Data not found'),
            '3' = status_row('circle-question', '#ed6c02', 'Data in invalid format')
          ),

          switch(
            substr(session$userData$conf$restore_status, 3, 3),
            '1' = status_row('check', '#2e7d32', 'Output restored successfully'),
            '2' = status_row('times', '#c62828', 'Output not found'),
            '3' = status_row('circle-question', '#ed6c02', 'Output in invalid format')
          )
        )

        showModal(modalDialog(
          title = div(
            icon('clock-rotate-left', style = 'margin-right:8px; color:#1565c0'),
            'Session Restore Status'
          ),
          div(style = '
                background:#f9fafb;
                padding:24px;
                border-radius:0px;
              ',
              list_check_restore
          ),

          size = 'l',
          easyClose = FALSE,

          footer = div(
            style = 'text-align:right;',
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
      dt = datasets,
      act_name = names(datasets[1]),
      bkp0 = NULL,
      bkp = NULL,
      meta = lapply(datasets, df_info)
    )

    session$userData$dt$bkp0 <- isolate(get_act_dt(session))

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    output$dt_act_name <- renderText(
      if(nchar(session$userData$dt$act_name) <= 20){
        session$userData$dt$act_name
      } else {
        paste0(substr(session$userData$dt$act_name, 1, 17) , '...')
      }
    )

    # datasets metadata -------------------------------------------------------
    session$userData$dt$data_changed <- reactiveVal(0)

    observe({
      req(session$userData$dt$dt)

      session$userData$dt$meta[[session$userData$dt$act_name]] <- get_act_dt(session) |> df_info()

    }) |> bindEvent(session$userData$dt$data_changed())

    session$userData$dt$gt_info <- reactive({
      req(session$userData$dt$meta)

      Map(gt_info, session$userData$dt$meta,
          df_name = names(session$userData$dt$meta))
    })

    # df active metadata ---------------------
    session$userData$dt$act_meta <- reactive({
      req(session$userData$dt$meta)

      session$userData$dt$meta[[session$userData$dt$act_name]]
    })

    # info to use in sidebar and navbar modules --------
    session$userData$dt$act_row_col <- reactive({
      req(session$userData$dt$act_meta())

      paste(session$userData$dt$act_meta() |> pull(rows) |> head(1) |> f_num(dig = 1),
            '/', session$userData$dt$act_meta() |> pull(cols) |> head(1) |> f_num())
    })

    session$userData$dt$act_col_nas <- reactive({
      req(session$userData$dt$act_meta())

      session$userData$dt$act_meta() |>
        filter(n_nas > 0) |>
        nrow()
    })

    session$userData$dt$act_size <- reactive({
      req(session$userData$dt$act_meta())
      (object.size(get_act_dt(session)) / 2^20) |>
        as.numeric() |> round(2)
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
      selectInput(
        'pD_data_sel_df',
        'Select a dataset',
        choices = c(
          session$userData$dt$act_name,
          setdiff(session$userData$dt_names(), session$userData$dt$act_name)
        )
      )
    })

    # make active dataset event --------------
    observe({
      session$userData$dt$act_name <- input$pD_data_sel_df
      session$userData$dt$bkp0 <- copy(get_act_dt(session))
      session$userData$dt$bkp <- NULL
      msg(paste('Dataset', session$userData$dt$act_name, 'is the active one'))
      updateTextInput(session, "pD_data_txt_new_name", value = '')
    }) |> bindEvent(input$pD_data_btn_active)

    # rename dataset event -------------------
    observe({
      if(!is_valid_name(input$pD_data_txt_new_name) |
         input$pD_data_txt_new_name %in% session$userData$dt_names()){
        msg_error('New name is not valid or already in use')
      } else {
        names(session$userData$dt$dt)[session$userData$dt_names() == input$pD_data_sel_df] <- input$pD_data_txt_new_name
        # update active dataset if necessary
        if(session$userData$dt$act_name == input$pD_data_sel_df){
          session$userData$dt$act_name <- input$pD_data_txt_new_name
          # update metadata names
          names(session$userData$dt$meta)[names(session$userData$dt$meta) == input$pD_data_sel_df] <- input$pD_data_txt_new_name
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
        session$userData$dt$dt[[input$pD_data_txt_new_name]] <- session$userData$dt$dt[[input$pD_data_sel_df]]
        # update metadata
        session$userData$dt$meta[[input$pD_data_txt_new_name]] <- session$userData$dt$meta[[input$pD_data_sel_df]]
        gc()

        msg(paste('Dataset', input$pD_data_txt_new_name, 'created'))

        updateTextInput(session, "pD_data_txt_new_name", value = '')
      }
    }) |> bindEvent(input$pD_data_btn_copy_dataset)

    # delete dataset event -------------------
    observe({
      if(session$userData$dt$act_name == input$pD_data_sel_df){
        msg_error('You can not delete the active dataset')
      } else {
        session$userData$dt$dt[[input$pD_data_sel_df]] <- NULL

        # delete metadata
        session$userData$dt$meta[[input$pD_data_sel_df]] <- NULL
        gc()
        msg(paste('Dataset', input$pD_data_sel_df, 'deleted'))

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
      update_act_dt(session, copy(session$userData$dt$bkp0))
      msg('Active Dataset Reseted')

      session$userData$dt$data_changed(session$userData$dt$data_changed() + 1)
    }) |> bindEvent(input$pE_btn_reset)

    # create backup --------------------------
    observe({
      session$userData$dt$bkp <- copy(get_act_dt(session))
      msg('Backup created')
    }) |> bindEvent(input$pE_btn_bkp)

    # restore backup -------------------------
    observe({
      if(is.null(session$userData$dt$bkp)){
        msg('No backup to restore')
      } else {
        update_act_dt(session, copy(session$userData$dt$bkp))
        msg('Backup restored')
        session$userData$dt$data_changed(session$userData$dt$data_changed() + 1)
      }
    }) |> bindEvent(input$pE_btn_restore)

    # clear backup ---------------------------
    observe({
      if(is.null(session$userData$dt$bkp)){
        msg('No backup to clear')
      } else {
        session$userData$dt$bkp <- NULL
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
    session$userData$out <- reactiveValues(elements = list())

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
