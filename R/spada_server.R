
# Function with the server of spada.R
spada_server <- function(datasets){
  function(input, output, session) {

    options(shiny.maxRequestSize = 500 * 1024 ^ 2)

    # data --------------------------------------------------------------------
    dt_react <- reactiveValues(data = lapply(datasets, setDT))
    dt_names_react <- reactive(names(dt_react$data))

    # inicialize with the first dataset informed
    df <- reactiveValues(
      df_active = copy(datasets[[1]]),
      df_active_name = names(datasets)[1],
      df_backup = NULL,
      df_trigger = 0
    )

    df_active_names <- reactive(df$df_active |> names())

    output$df_active_name <- renderText(
      if(nchar(df$df_active_name) <= 20){
        df$df_active_name
      } else {
        paste0(substr(df$df_active_name, 1, 17) , '...')
      }
    )

    # navbar ------------------------------------------------------------------
    # info used in navbar and sidebar
    df_active_resume_data <- reactiveValues()

    observe({
      df_active_resume_data$nrow <- df_metadata() |> pull(rows) |> head(1)
      df_active_resume_data$ncol <- df_metadata() |> pull(cols) |> head(1)
      df_active_resume_data$name <- df$df_active_name
      df_active_resume_data$n_nas <- df_metadata() |> filter(n_nas > 0) |> nrow()
      df_active_resume_data$size <- (object.size(df$df_active) / 2^20) |>
        as.numeric() |> round(2)
    })

    navbar_df_info_server('navbar_df_info',
                          reactive(df_active_resume_data),
                          app_session = session)

    # side bar --------------------------------------------------------
    sidebar_server('sidebar', reactive(df_active_resume_data), app_session = session)

    # data page events -----------------------------------------------------
    df_metadata <- reactive({
      df$df_trigger
      df_info(df$df_active)
    })

    output$pD_metadata_gt <- render_gt(df_metadata() |> gt_info())

    # overview -----------------------
    data_overview_server(
      'pD_overview', reactive(df$df_active),
       reactive(list(mod_pE_convert_cols$df_convert_cols_trigger(),
                     mod_pE_order_rows$btn_order_rows(),
                     mod_pE_order_cols$btn_order_cols())
                )
    )
    # values for boxes -----------------------
    data_highlights_server('pD_highlights', reactive(df$df_active), df_metadata)

    # define active dataset ---------------------------------------------------
    output$pD_data_ui_sel_df <- renderUI(
      selectInput('pD_data_sel_df', 'Select a dataset', dt_names_react())
    )

    observe({
      # save active to original data
      dt_react$data[[df$df_active_name]] <- df$df_active
      # choose new dataset to be active
      df$df_active <- dt_react$data[[input$pD_data_sel_df]]
      df$df_backup <- NULL

      df$df_active_name <- input$pD_data_sel_df

      msg(paste('Dataset', df$df_active_name, 'is the active one'))
      gc()
    }) |> bindEvent(input$pD_data_btn_active)

    observe({
      if(!is_valid_name(input$pD_data_txt_new_name) |
         input$pD_data_txt_new_name %in% dt_names_react()){
        msg_error('New name is not valid or already in use')
      } else {
        names(dt_react$data)[dt_names_react() == input$pD_data_sel_df] <- input$pD_data_txt_new_name
        # update active dataset if necessary
        if(df$df_active_name == input$pD_data_sel_df){
          df$df_active <- dt_react$data[[input$pD_data_txt_new_name]]

          df$df_active_name <- input$pD_data_txt_new_name
        }
        msg('New name applied')
      }
    }) |> bindEvent(input$pD_data_btn_new_name)

    observe({
      if(!is_valid_name(input$pD_data_txt_new_name) ||
         (input$pD_data_txt_new_name %in% dt_names_react())){
        msg_error('New name is not valid or already in use')
      } else {
        dt_react$data[[ input$pD_data_txt_new_name ]] <- df$df_active
        msg(paste('Dataset', input$pD_data_txt_new_name, 'created'))
        gc()
      }
    }) |> bindEvent(input$pD_data_btn_copy_dataset)

    observe({
      if(df$df_active_name == input$pD_data_sel_df){
        msg_error('You can not delete the active dataset')
      } else {
        dt_react$data[[ input$pD_data_sel_df ]] <- NULL
        msg(paste('Dataset', input$pD_data_sel_df, 'deleted'))
        gc()
      }
    }) |> bindEvent(input$pD_data_btn_delete_dataset)

    # export file ----------------------------------------------------
    export_file_server('pD_export', reactive(df$df_active))

    # import file ----------------------------------------------------
    mod_pD_import <- import_file_server('pD_import', dt_names_react)

    observe({
      req(mod_pD_import$data_imported())

      dt_react$data[[mod_pD_import$data_imported()[['data_name']]]] <- mod_pD_import$data_imported()[['data']]

    }) |> bindEvent(mod_pD_import$data_imported())

    # edit page events --------------------------------------------------------

    # filter events ---------------------------
    mod_pE_filter_rows <- filter_rows_server('pE_filter_rows',
                                             reactive(df$df_active))
    # update df_active after sel_cols
    observe({
      req(mod_pE_filter_rows$df_filter_rows())

      df$df_active <- mod_pE_filter_rows$df_filter_rows()

    }) |> bindEvent(mod_pE_filter_rows$df_filter_rows())

    # select cols ---------------------------
    mod_pE_sel_cols <- select_cols_server('pE_filter_sel_cols',
                                          reactive(df$df_active))
    # update df_active after sel_cols
    observe({
      req(mod_pE_sel_cols$df_sel_cols())

      df$df_active <- mod_pE_sel_cols$df_sel_cols()

    }) |> bindEvent(mod_pE_sel_cols$df_sel_cols())

    # convert events ---------------------------
    mod_pE_convert_cols <- convert_cols_server(
      'pE_convert_cols',
      reactive(df$df_active),
      reactive(df$df_trigger))

    # update df_active
    observe({
      req(mod_pE_convert_cols$df_convert_cols())
      req(mod_pE_convert_cols$df_convert_cols_trigger())

      df$df_active <- mod_pE_convert_cols$df_convert_cols()
      df$df_trigger <- mod_pE_convert_cols$df_convert_cols_trigger()

    }) |> bindEvent(mod_pE_convert_cols$df_convert_cols(),
                    mod_pE_convert_cols$df_convert_cols_trigger())

    # order rows events ---------------------------
    mod_pE_order_rows <- order_rows_server('pE_order_rows', reactive(df$df_active))

    # update df_active
    observe({
      req(mod_pE_order_rows$df_order_rows())

      df$df_active <- mod_pE_order_rows$df_order_rows()

    }) |> bindEvent(mod_pE_order_rows$df_order_rows())

    # order cols events ---------------------------
    mod_pE_order_cols <- order_cols_server('pE_order_cols', reactive(df$df_active))
    # update df_active
    observe({
      req(mod_pE_order_cols$df_order_cols())

      df$df_active <- mod_pE_order_cols$df_order_cols()

    }) |> bindEvent(mod_pE_order_cols$df_order_cols())

    # rename cols events ---------------------------
    mod_pE_rename_cols <- rename_cols_server('pE_rename_cols', reactive(df$df_active))
    # update df_active
    observe({
      req(mod_pE_rename_cols$df_rename_cols())
      df$df_active <- mod_pE_rename_cols$df_rename_cols()

    }) |> bindEvent(mod_pE_rename_cols$df_rename_cols())

    # reset df active ---------------------------
    observe({
      df$df_active <- copy(dt_react$data[[df$df_active_name]])
      msg('Active Dataset Reseted')
    }) |> bindEvent(input$pE_btn_reset)

    # create backup ---------------------------
    observe({
      df$df_backup <- copy(df$df_active)
      msg('Backup created')
    }) |> bindEvent(input$pE_btn_bkp)

    # restore backup ---------------------------
    observe({
      if(is.null(df$df_backup)){
        msg('No backup to restore')
      } else {
        df$df_active <- copy(df$df_backup)
        msg('Backup restored')
      }
    }) |> bindEvent(input$pE_btn_restore)

    # clear backup ---------------------------
    observe({
      if(is.null(df$df_backup)){
        msg('No backup to clear')
      } else {
        df$df_backup <- NULL
        msg('Backup cleared')
      }
    }) |> bindEvent(input$pE_btn_clear_bkp)

    # analysis page events ----------------------------------------------------
    exploratory_server('pA_exploratory', reactive(df$df_active), df_metadata,
                       color_fill, color_line)

    # descriptive stats -------------------------------------------------------
    descriptive_stats_server('pA_desc_stats', reactive(df$df_active))

    # correlation -------------------------------------------------------------
    correlation_server('pA_correlation', reactive(df$df_active), df_metadata,
                       color_fill)

    # normality test ----------------------------------------------------------
    normality_test_server('pA_normality_test', reactive(df$df_active),
                       df_metadata, color_fill, color_line)

    # normality test ----------------------------------------------------------
    z_test_server('pA_z_test', reactive(df$df_active), df_metadata,
                  color_fill, color_line)

    # config events -----------------------------------------------------------
    mod_pC <- page_config_server('pC')
    color_fill <- reactive(mod_pC$palette()[['fill']])
    color_line <- reactive(mod_pC$palette()[['line']])

    # exit app event ----------------------------------------------------------
    session$onSessionEnded(stopApp)
    observe({
      if (input$navbar == 'exit') {
        session$sendCustomMessage(type = 'closeWindow', message = 'message')
        stopApp()
      }
    })
  }
}
