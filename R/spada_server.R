
# Function with the server of spada.R
spada_server <- function(datasets){
  function(input, output, session) {

    options(shiny.maxRequestSize = 500 * 1024 ^ 2)

    # data --------------------------------------------------------------------
    datasets_react <- reactiveValues(data = lapply(datasets, setDT))
    datasets_names_react <- reactive(names(datasets_react$data))

    # inicialize with the first dataset informed
    df <- reactiveValues(
      df_active = copy(datasets[[1]]),
      df_active_name = names(datasets)[1],
      df_backup = NULL,
      df_trigger = 0
    )

    gc()
    df_active_names <- reactive(df$df_active |> names())
    df_active_nrow <- reactive(df$df_active |> nrow())
    df_active_ncol <- reactive(df$df_active |> ncol())

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
      df_active_resume_data$nrow <- df_active_nrow()
      df_active_resume_data$ncol <- df_active_ncol()
      df_active_resume_data$name <- df$df_active_name
      df_active_resume_data$n_nas <- df_metadata() |> filter(n_nas > 0) |> nrow()
      df_active_resume_data$size <- (object.size(df$df_active) / 2^20) |>
        as.numeric() |> round(2)
    })

    navbar_df_info_server('navbar_df_info', reactive(df_active_resume_data),
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
    data_overview_server('pD_overview', reactive(df$df_active),
                         reactive(list(input$pE_convert_btn_apply,
                                       input$pE_order_btn_order_rows,
                                       mod_pE_order_cols$btn_order_cols()
                                       )))

    # values for boxes -----------------------
    data_highlights_server('pD_highlights', reactive(df$df_active), df_metadata)

    # define active dataset ---------------------------------------------------
    output$pD_data_ui_sel_df <- renderUI(
      selectInput('pD_data_sel_df', 'Select a dataset', datasets_names_react())
    )

    observe({
      # save active to original data
      datasets_react$data[[df$df_active_name]] <- df$df_active
      # choose new dataset to be active
      df$df_active <- datasets_react$data[[input$pD_data_sel_df]]
      df$df_backup <- NULL

      df$df_active_name <- input$pD_data_sel_df

      msg(paste('Dataset', df$df_active_name, 'is the active one'))
    }) |> bindEvent(input$pD_data_btn_active)

    observe({
      if(!is_valid_name(input$pD_data_txt_new_name) |
         input$pD_data_txt_new_name %in% datasets_names_react()){
        msg_error('New name is not valid or already in use')
      } else {
        names(datasets_react$data)[datasets_names_react() == input$pD_data_sel_df] <- input$pD_data_txt_new_name
        # update active dataset if necessary
        if(df$df_active_name == input$pD_data_sel_df){
          df$df_active <- datasets_react$data[[input$pD_data_txt_new_name]]

          df$df_active_name <- input$pD_data_txt_new_name
        }
        msg('New name applied')
      }
    }) |> bindEvent(input$pD_data_btn_new_name)

    observe({
      if(!is_valid_name(input$pD_data_txt_new_name) ||
         (input$pD_data_txt_new_name %in% datasets_names_react())){
        msg_error('Name invalid or already in use')
      } else {
        datasets_react$data[[ input$pD_data_txt_new_name ]] <- df$df_active
        msg(paste('Dataset', input$pD_data_txt_new_name, 'created'))
      }
    }) |> bindEvent(input$pD_data_btn_copy_dataset)

    observe({
      if(df$df_active_name == input$pD_data_sel_df){
        msg_error('You can not delete the active dataset')
      } else {
        datasets_react$data[[ input$pD_data_sel_df ]] <- NULL
        msg(paste('Dataset', input$pD_data_sel_df, 'deleted'))
      }
    }) |> bindEvent(input$pD_data_btn_delete_dataset)

    # export ----------------------------------------------------
    export_file_server('pD_export', reactive(df$df_active))

    # import ----------------------------------------------------
    mod_pD_import <- import_file_server('pD_import', datasets_names_react())

    observe({
      req(mod_pD_import$data_imported())

      datasets_react$data[[mod_pD_import$data_imported()[['data_name']]]] <- mod_pD_import$data_imported()[['data']]

    }) |> bindEvent(mod_pD_import$data_imported())

    # edit page events --------------------------------------------------------

    # filter events ---------------------------
    output$pE_filter_ui_var_filter <- renderUI(
      selectInput('pE_filter_vars_filter', 'Variable', c('', df_active_names()))
    )

    observe({
      output$pE_filter_ui_value <- renderUI({
        if (df$df_active[[input$pE_filter_vars_filter]] |> is_date() &
            input$pE_filter_operator %in% c('==', '!=', '>', '>=', '<', '<=', 'is_na', 'not_na')){
          dateInput('pE_filter_value', 'Date')
        } else if (df$df_active[[input$pE_filter_vars_filter]] |> is_date() &
                   input$pE_filter_operator %in% c('between', 'not_between')){
          dateRangeInput('pE_filter_value', 'Date')
        } else {
          selectizeInput(
            'pE_filter_value', 'Value',
            choices = NULL,
            multiple = T,
            options = list(create = T)
          ) |> tooltip('Text should not be in quotes', placement = 'top')
        }
      })
    }) |> bindEvent(input$pE_filter_vars_filter)

    pE_filter_value_temp <- reactive({

      req(input$pE_filter_value)

      if(df$df_active[[input$pE_filter_vars_filter]] |> is.numeric()){
        unlist(input$pE_filter_value) |> as.numeric()
      } else if (df$df_active[[input$pE_filter_vars_filter]] |> is_date()){
        unlist(input$pE_filter_value) |> as.Date()
      } else if (df$df_active[[input$pE_filter_vars_filter]] |> is.raw()){
        unlist(input$pE_filter_value) |> as.raw()
      } else if (df$df_active[[input$pE_filter_vars_filter]] |> is.complex()){
        unlist(input$pE_filter_value) |> as.complex()
      } else {
        input$pE_filter_value
      }
    })

    # update selectinput to show pertinent operators
    observe({
      updateSelectInput(
        session, 'pE_filter_operator',
        label = 'Operator',
        choices =
          if(df$df_active[[input$pE_filter_vars_filter]] |> is.factor() ||
             df$df_active[[input$pE_filter_vars_filter]] |> is.character() ||
             df$df_active[[input$pE_filter_vars_filter]] |> is.complex()
          ){
            c('',
              '== (Equal)' = '==',
              '!= (Not Equal)' = '!=',
              'Is NA (is.na)' = 'is_na',
              'Not NA (! is.na)' = 'not_na',
              'In (%in%)' = 'in',
              'Not In (! %in%)' = 'not_in')
          } else { c('', filter_operators) }
      )
    }) |> bindEvent(input$pE_filter_vars_filter)

    # update current format txt
    observe({
      updateTextInput(session, 'pE_filter_txt_preview_value',
                      label = 'Preview value',
                      value = pE_filter_value_temp()
      )
    }) |> bindEvent(pE_filter_value_temp())

    # filter rows
    observe({
      if(input$pE_filter_vars_filter == '' || input$pE_filter_operator == ''
         || length(pE_filter_value_temp()) == 0){
        msg('Choose a variable, an operator and a value', 3)
      } else if(length(pE_filter_value_temp()) > 1 & input$pE_filter_operator %in%
                c('==', '!=', '>', '>=', '<', '<=')){
        msg_error('Operator requires value of length 1')
      } else if(length(pE_filter_value_temp()) != 2 & input$pE_filter_operator %in%
                c('between', 'not_between')){
        msg_error('Operator requires value of length 2')
      } else {
        df$df_active <- filter_rows(df$df_active,
                                    input$pE_filter_vars_filter,
                                    input$pE_filter_operator,
                                    pE_filter_value_temp())
        msg('Filter rows: OK')
      }
    }) |> bindEvent(input$pE_filter_btn_filter)

    # update selectize for factors
    observe({
      req(input$pE_filter_vars_filter)
      if(df$df_active[[input$pE_filter_vars_filter]] |> is.factor()){
        updateSelectizeInput(
          session,
          'pE_filter_value',
          label = 'Value',
          choices = df$df_active[[input$pE_filter_vars_filter]] |> levels(),
          selected = ''
        )
      }
    })

    # clear value after click in button
    observe({
      updateSelectizeInput(
        session,
        'pE_filter_value',
        label = 'Value',
        choices = NULL,
        selected = ''
      )
    }) |> bindEvent(input$pE_filter_btn_filter)

    # select cols ---------------------------
    mod_pE_sel_cols <- select_cols_server('pE_filter_sel_cols',
                                          reactive(df$df_active))
    # update df_active after sel_cols
    observe({
      req(mod_pE_sel_cols$df_sel_cols())

      df$df_active <- mod_pE_sel_cols$df_sel_cols()

    }) |> bindEvent(mod_pE_sel_cols$df_sel_cols())

    # convert events ---------------------------
    output$pE_convert_ui_var_sel <- renderUI(
      selectInput('pE_convert_vars_sel', 'Variable', c('', df_active_names()))
    )

    current_format <- reactive({
      req(input$pE_convert_vars_sel)
      paste('Type: [', df$df_active[[input$pE_convert_vars_sel]] |> typeof(), '] |',
            'Class: [',
            paste(df$df_active[[input$pE_convert_vars_sel]] |> class(), collapse = '/'),
            ']')
    })
    # update current format txt
    observe({
      updateTextInput(session, 'pE_convert_txt_current_format',
                      label = 'Current Type / Class',
                      value = current_format()
      )
    }) |> bindEvent(current_format())

    # sample to preview conversion
    pE_convert_preview_sample_trigger <- reactiveVal(1)
    pE_convert_preview_sample <- reactive({
      pE_convert_preview_sample_trigger()
      if(df_active_nrow() < 8) {
        rep(TRUE, df_active_nrow())
      } else {
        sample(df_active_nrow(), 8, replace = F)
      }
    })

    # update sample in button click
    observe({
      pE_convert_preview_sample_trigger(pE_convert_preview_sample_trigger() + 1)
    }) |> bindEvent(input$pE_convert_btn_preview_sample)

    pE_convert_preview_df <- reactive({
      req(input$pE_convert_vars_sel)
      req(input$pE_convert_sel_format)

      if(input$pE_convert_sel_format == 'as.Date'){
        req(input$pE_convert_sel_date_formats)
      }

      pE_convert_preview_df_temp <- subset(
        df$df_active[pE_convert_preview_sample(), ],
        select = input$pE_convert_vars_sel)

      pE_convert_preview_df_temp[
        , preview := convert(get(input$pE_convert_vars_sel),
                             type = input$pE_convert_sel_format,
                             date_format = input$pE_convert_sel_date_formats,
                             date_origin = input$pE_convert_sel_date_origin)]
    })

    output$pE_convert_preview_gt1 <- render_gt({
      req(input$pE_convert_vars_sel)
      if(input$pE_convert_vars_sel %in% names(df$df_active)){
        pE_convert_preview_df() |>
          lapply(\(x) if(is.complex(x)) as.character(x) else x) |>
          as.data.frame() |>
          gt() |>
          opt_interactive(
            use_sorting = F,
            use_pagination = F,
            use_highlight = T,
            use_compact_mode = T) |>
          tab_options(table.background.color = '#f9f9f9')
      }
    })

    observe({
      if(input$pE_convert_vars_sel == '' | input$pE_convert_sel_format == ''){
        msg('Choose a variable and a new format')
      } else {
        df$df_active[, input$pE_convert_vars_sel :=
                       convert(get(input$pE_convert_vars_sel),
                               type = input$pE_convert_sel_format,
                               date_format = input$pE_convert_sel_date_formats,
                               date_origin = input$pE_convert_sel_date_origin)]
        msg('Conversion applied')
      }

      df$df_trigger <- df$df_trigger + 1
    }) |> bindEvent(input$pE_convert_btn_apply)

    # order rows events ---------------------------
    output$pE_order_ui_var_rows <- renderUI(
      selectInput('pE_order_vars_rows', 'Order by', c('', df_active_names()), multiple = T)
    )

    # vars in descending order
    observe({
      updateSelectInput(
        session,
        'pE_order_vars_descending',
        label = 'Descending Order',
        choices = input$pE_order_vars_rows
      )
    }) |> bindEvent(input$pE_order_vars_rows)

    # btn order rows ---------------------------
    observe({
      pE_order_rows_position <- rep(1, input$pE_order_vars_rows |> length())

      pE_order_rows_position[which(input$pE_order_vars_rows %in% input$pE_order_vars_descending)] <- -1

      setorderv(df$df_active, cols = input$pE_order_vars_rows,
                order = pE_order_rows_position,
                na.last = if(input$pE_order_radio_nas == 'last'){
                  TRUE
                } else if (input$pE_order_radio_nas == 'first'){
                  FALSE
                } else { FALSE }
      )
      msg('Reordering Rows: OK')
    }) |> bindEvent(input$pE_order_btn_order_rows)

    # order cols events ---------------------------
    mod_pE_order_cols <- order_cols_server('pE_filter_order_cols',
                                          reactive(df$df_active))
    # update df_active after sel_cols
    observe({
      req(mod_pE_order_cols$df_order_cols())

      df$df_active <- mod_pE_order_cols$df_order_cols()

    }) |> bindEvent(mod_pE_order_cols$df_order_cols())

    # reset df active ---------------------------
    observe({
      df$df_active <- copy(datasets_react$data[[df$df_active_name]])
      gc()
      msg('Active Dataset Reseted')
    }) |> bindEvent(input$pE_btn_reset)

    # create backup ---------------------------
    observe({
      df$df_backup <- copy(df$df_active)
      gc()
      msg('Backup created')
    }) |> bindEvent(input$pE_btn_bkp)

    # restore backup ---------------------------
    observe({
      if(is.null(df$df_backup)){
        msg('No backup to restore')
      } else {
        df$df_active <- copy(df$df_backup)
        gc()
        msg('Backup restored')
      }
    }) |> bindEvent(input$pE_btn_restore)

    # clear backup ---------------------------
    observe({
      if(is.null(df$df_backup)){
        msg('No backup to clear')
      } else {
        df$df_backup <- NULL
        gc()
        msg('Backup cleared')
      }
    }) |> bindEvent(input$pE_btn_clear_bkp)

    # analysis page events ----------------------------------------------------
    var_analysis <- reactive({
      df_metadata() |> filter(perc_nas != 1) |>  pull(var)
    })

    output$pA_E_ui_var_names <- renderUI(
      selectInput('pA_E_sel_vars', 'Main Variable', var_analysis()) |>
        tooltip('Dependent Variable', placement = 'top')
    )

    output$pA_E_ui_var_names2 <- renderUI(
      selectInput('pA_E_sel_vars2', 'Variable 2', var_analysis(), var_analysis()[2]) |>
        tooltip('Independent Variable', placement = 'top')
    )

    pA_E_outliers_index <- reactive({
      v <- df$df_active[[input$pA_E_sel_vars]]
      if(input$pA_E_outliers & is.numeric(v)) {
        q1 <- p25(v)
        q3 <- p75(v)
        dist_interquatile <- q3 - q1
        v >= (q1 - 1.5 * dist_interquatile) & v <= (q3 + 1.5 * dist_interquatile)
      } else {
        rep(T, length(v))
      }
    })

    # values to analysis page -------------------------------------------------
    pA_E_var <- reactive({
      req(input$pA_E_sel_vars)
      df$df_active[[input$pA_E_sel_vars]][pA_E_outliers_index()]
    })

    pA_E_var2 <- reactive({
      req(input$pA_E_sel_vars2)
      df$df_active[[input$pA_E_sel_vars2]][pA_E_outliers_index()]
    })

    pA_E_var_percentile <- reactive(
      if(isTruthy(input$pA_E_var_percentile) && is.numeric(pA_E_var()) &&
         between(input$pA_E_var_percentile, 0, 100)){
        pn(pA_E_var(), input$pA_E_var_percentile / 100)
      } else { NA }
    ) |> bindCache(pA_E_var(), input$pA_E_var_percentile)

    # render plots ------------------------------------------------------------
    output$pA_E_g_dist <- renderPlot({
      if (input$pA_E_radio_dist_plot == 'barplot'){
        validate(need(!is.numeric(pA_E_var()), 'Var can not be numeric'))
        barplot(table(pA_E_var()), col = color_fill())
      } else {
        validate(need(is.numeric(pA_E_var()), 'Var must be numeric'))

        if (input$pA_E_radio_dist_plot == 'boxplot_group'){
          pA_E_g_dist_boxg_col <- colors()[sample.int(
            colors() |> length(), unique(pA_E_var2()) |> length(), replace = F)]

          boxplot(pA_E_var() ~ pA_E_var2(), horizontal = T,
                  col = pA_E_g_dist_boxg_col, xlab = '', ylab = '')
          abline(v = pA_E_var_percentile(), col = color_line())
        } else {
          validate(
            need(isTruthy(input$pA_E_var_percentile)
                 && between(input$pA_E_var_percentile, 0, 100), 'Percentile must be between 0 and 100')
          )

          if(input$pA_E_radio_dist_plot == 'hist'){
            validate(need(input$pA_E_bins > 0, 'Bins must be 1 or higher'))
            hist(pA_E_var(),
                 col = color_fill(),
                 breaks = input$pA_E_bins,
                 main = '',
                 xlab = '',
                 ylab = 'Count')
            abline(v = pA_E_var_percentile(), col = color_line(), lwd = 2)
          } else if (input$pA_E_radio_dist_plot == 'boxplot'){
            boxplot(pA_E_var(), horizontal = T, col = color_fill())
            abline(v = pA_E_var_percentile(), col = color_line(), lwd = 2)
          } else if (input$pA_E_radio_dist_plot == 'dots'){
            plot(pA_E_var(), col = color_fill(), ylab = 'Values', pch = 19)
            abline(h = pA_E_var_percentile(), col = color_line(), lwd = 2)
          }
        }
      }
    }) |> bindCache(pA_E_var(), pA_E_var2(), input$pA_E_radio_dist_plot, input$pA_E_bins,
                    input$pA_E_var_percentile, color_fill(), color_line())
    # render scatter plot -----------------------------------------------------
    output$pA_E_g_scatter <- renderPlot({
      if (input$pA_E_scatter_lm &
          pA_E_linear_model$y_name == input$pA_E_sel_vars &
          pA_E_linear_model$x_name == input$pA_E_sel_vars2) {
        plot(
          pA_E_var2(),
          pA_E_var(),
          type = 'p',
          col = color_fill(),
          xlab = input$pA_E_sel_vars2,
          ylab = input$pA_E_sel_vars,
          pch = 19
        )
        lines(
          pA_E_linear_model$x,
          pA_E_linear_model$y,
          col = color_line(),
          lty = 'dotdash',
          lwd = 2
        )
        mtext(paste('Adjusted R Squared:',
                    summary(pA_E_linear_model$model)$r.squared |> round(4)),
              side = 3)
      } else {
        plot(
          pA_E_var2(),
          pA_E_var(),
          type = 'p',
          col = color_fill(),
          xlab = input$pA_E_sel_vars2,
          ylab = input$pA_E_sel_vars,
          pch = 19
        )
        mtext(paste('Pearson Correlation:', pA_E_stats_correlation() |> round(4)))
      }
    }) |> bindCache(
      input$pA_E_scatter_lm,
      pA_E_linear_model$y_name,
      pA_E_linear_model$x_name,
      input$pA_E_sel_vars,
      input$pA_E_sel_vars2,
      pA_E_var2(),
      pA_E_var(),
      pA_E_linear_model$x,
      pA_E_linear_model$y,
      color_fill(),
      color_line()
    ) |> bindEvent(input$pA_E_btn_scatter)

    # tables ------------------------------------------------------------------
    output$pA_E_table <- renderPrint(
      if(input$pA_E_table_type == '1d') {
        table(pA_E_var())
      } else if (input$pA_E_table_type == '2d'){
        table(pA_E_var(), pA_E_var2())
      }
    )|> bindCache(
      pA_E_var(),
      pA_E_var2(),
      input$pA_E_table_type)

    # linear model ------------------------------------------------------------
    pA_E_linear_model <- reactiveValues(
      model = NULL,
      x = NULL,
      y = NULL,
      x_name = '',
      y_name = ''
    )

    observe({
      if(!is.numeric(pA_E_var())){
        msg('The Dependent variable must be numeric', 2.5)
      } else if (input$pA_E_sel_vars == input$pA_E_sel_vars2) {
        msg('Choose diferent variables for X and Y.', 2.5)
      } else {
        pA_E_linear_model$y_name <- input$pA_E_sel_vars
        pA_E_linear_model$x_name <- input$pA_E_sel_vars2

        pA_E_var_size <- length(pA_E_var())

        if(pA_E_var_size < 10e3) {
          var_y <- pA_E_var()
          var_x <- pA_E_var2()
        } else {
          pA_E_sample_size <- min(pA_E_var_size,
                                  floor(pA_E_var_size * min(1, max(0, input$pA_E_sample_size/100))))
          lm_sample <- sample.int(pA_E_var_size, pA_E_sample_size, replace = F) |>
            sort()
          var_y <- pA_E_var()[lm_sample]
          var_x <- pA_E_var2()[lm_sample]
        }

        pA_E_linear_model$model <- lm(var_y ~ var_x, model = F)
        pA_E_linear_model$x <- var_x
        pA_E_linear_model$y <- pA_E_linear_model$model$fitted.values
        msg('Lm model completed.')
      }
    }) |> bindEvent(input$pA_E_btn_scatter_lm_run)

    observe({
      pA_E_linear_model$model <- NULL
      pA_E_linear_model$x <- NULL
      pA_E_linear_model$y <- NULL
      pA_E_linear_model$x_name <-
        pA_E_linear_model$y_name <- ''
      msg('Lm model cleared.')
    }) |> bindEvent(input$pA_E_btn_scatter_lm_clear)

    # print linear model ------------------------------------------------------
    output$pA_E_linear_model <- renderPrint({
      list(
        'Formula' = paste(pA_E_linear_model$y_name, '~', pA_E_linear_model$x_name),
        'Model' = summary(pA_E_linear_model$model)
      )
    }) |> bindCache(pA_E_linear_model$y_name,
                    pA_E_linear_model$x_name,
                    pA_E_linear_model$model)

    # plot linear model residuals ---------------------------------------------
    output$pA_E_g_lm_resid <- renderPlot({

      validate(need(isTruthy(pA_E_linear_model$model), 'No residuals to plot'))

      if(input$pA_E_radio_lm_resid == 'hist'){
        hist(pA_E_linear_model$model$residuals,
             col = color_fill(),
             main = '',
             xlab = '',
             ylab = 'Count')
      } else if (input$pA_E_radio_lm_resid == 'boxplot'){
        boxplot(pA_E_linear_model$model$residuals,
                horizontal = T, col = color_fill())
      } else if (input$pA_E_radio_lm_resid == 'dots'){
        plot(pA_E_linear_model$model$residuals, col = color_fill(),
             ylab = 'Residuals', pch = 19)
        abline(h = 0, col = color_line(), lty = 'dotdash', lwd = 2)
      }
    }) |> bindEvent(input$pA_E_btn_lm_resid)

    # metrics -----------------------------------------------------------------
    pA_E_stats_sd <- reactive(if(is.numeric(pA_E_var())) sd(pA_E_var(), na.rm = T) else NA)

    pA_E_stats_correlation <- reactive(
      if(is.numeric(pA_E_var()) && is.numeric(pA_E_var2()) && pA_E_stats_sd() != 0 &&
         !is.na(pA_E_stats_sd())){
        sd_pA_E_var2 <- sd(pA_E_var2(), na.rm = T)
        if(sd_pA_E_var2 == 0 || sd_pA_E_var2 |> is.na()) {
          NA
        } else {
          cor(pA_E_var(), pA_E_var2(), method = 'p', use = 'na.or.complete')
        }
      } else { NA }
    )
    # stats table -------------------------------------------------------------
    stats_table_server('pA_stats', pA_E_var, pA_E_var2,
                       reactive(input$pA_E_var_percentile),
                       pA_E_var_percentile,
                       pA_E_stats_sd, pA_E_stats_correlation)

    # descriptive stats -------------------------------------------------------
    descriptive_stats_server('pA_desc_stats', reactive(df$df_active))

    # correlation -------------------------------------------------------------
    correlation_server('pA_correlation', reactive(df$df_active), df_metadata, color_fill)

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
