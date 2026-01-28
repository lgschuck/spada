
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

        display_restore_status(session$userData$conf$restore_status)

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
      meta = lapply(datasets, df_info),
      updated_cols = NULL,
      data_changed_rename = FALSE,
      data_changed_reorder = FALSE
    )

    session$userData$data_changed <- reactiveVal(0)

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
    observe({
      req(session$userData$dt$dt)

      if(is.null(session$userData$dt$updated_cols)) {
        # update meta
        session$userData$dt$meta[[session$userData$dt$act_name]] <- get_act_dt(session) |>
          df_info()
      } else {

        if(session$userData$dt$data_changed_rename){
          session$userData$dt$meta[[session$userData$dt$act_name]]$var <- get_act_dt(session) |> names()
        } else if (session$userData$dt$data_changed_reorder){
          order_df <- data.frame(var = get_act_dt(session) |> names(),
                                 index = 1:length(get_act_dt(session) |> names()))

          meta_reordered <- merge(session$userData$dt$meta[[session$userData$dt$act_name]],
                                  order_df, by = 'var', all.x = TRUE)

          setorderv(meta_reordered, c('index'))

          meta_reordered$index <- NULL
          session$userData$dt$meta[[session$userData$dt$act_name]] <- meta_reordered

        } else {

          session$userData$dt$meta[[session$userData$dt$act_name]] <- update_meta(
            dt = get_act_dt(session)[, .SD, .SDcols = session$userData$dt$updated_cols],
            previous_meta = session$userData$dt$meta[[session$userData$dt$act_name]],
            col_names = get_act_dt(session) |> names(),
            updated_cols = session$userData$dt$updated_cols,
            ncols = get_act_dt(session) |> ncol()
          )
        }

        session$userData$dt$updated_cols <- NULL
        session$userData$dt$data_changed_rename <- FALSE
        session$userData$dt$data_changed_reorder <- FALSE
      }

    }) |> bindEvent(session$userData$data_changed())

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
    session$userData$dt$act_mini_meta <- reactive({
      req(session$userData$dt$act_meta())

      list(
        'row_col' = paste(session$userData$dt$act_meta() |>
                                pull(rows) |> head(1) |> f_num(dig = 1),
                              '/',
                              session$userData$dt$act_meta() |>
                                pull(cols) |> head(1) |> f_num()),
        'col_nas' = session$userData$dt$act_meta() |>
          filter(n_nas > 0) |> nrow(),
        'size' = (object.size(get_act_dt(session)) / 2^20) |>
          as.numeric() |> round(2)
      )
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

    # manage datasets ------------------------
    data_server('pD_data')

    # export file ----------------------------
    export_file_server('pD_export')

    # import file ----------------------------
    import_file_server('pD_import')

    # edit page events --------------------------------------------------------

    # calculate cols events ------------------
    calculate_cols_server('pE_calculate_cols')

    # convert events -------------------------
    convert_cols_server('pE_convert_cols')

    # filter events --------------------------
    filter_rows_server('pE_filter_rows')

    # select cols ----------------------------
    select_cols_server('pE_filter_sel_cols')

    # order rows events ----------------------
    order_rows_server('pE_order_rows')

    # order cols events ----------------------
    order_cols_server('pE_order_cols')

    # rename cols events ---------------------
    rename_cols_server('pE_rename_cols')

    # summarise events -----------------------
    summarise_server('pE_summarise')

    # edit backup events ---------------------
    data_bkp_server('pE_bkp')

    # analysis page events ----------------------------------------------------
    exploratory_server('pA_exploratory')

    # descriptive stats ----------------------
    descriptive_stats_server('pA_desc_stats')

    # correlation ----------------------------
    correlation_server('pA_correlation')

    # normality test -------------------------
    normality_test_server('pA_normality_test')

    # z test ---------------------------------
    z_test_server('pA_z_test')

    # linear model ---------------------------
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
