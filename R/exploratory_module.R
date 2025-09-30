
# ui --------------------------------------------------------------------------
exploratory_ui <- function(id) {
  ns <- NS(id)

  card(
    full_screen = T,
    card_body(
      class = 'big-card',
      layout_columns(
        col_widths = c(2, 7, 3),
        navset_card_pill(
          full_screen = T,
          nav_panel('Parameters',
                    uiOutput(ns('ui_var_names')),
                    uiOutput(ns('ui_var_names2'))),
          nav_panel('Filters',
                    checkboxInput(ns('outliers'),
                                  list('Remove Outliers', bs_icon('info-circle')) |>
                                    ttip('Only for numeric vars'))
          )
        ),
        navset_card_pill(
          full_screen = T,
          nav_panel(
            'Distribution',
            full_screen = T,
            card_body(plotOutput(ns('dist_plot'))),
            card_footer(
              fluidRow(
                column(6,
                       radioGroupButtons(
                         ns('radio_dist_plot'),
                         'Plot type:',
                         c('Histogram' = 'hist',
                           'Boxplot' = 'boxplot',
                           'Boxplot by Groups' = 'boxplot_group',
                           'Dots' = 'dots',
                           'Barplot' = 'barplot'), size = 'sm', individual = T)),
                column(2, numericInput(ns('var_percentile'), 'Percentile', 50, 0, 100, 5)),
                column(1, conditionalPanel(
                  condition = "input.radio_dist_plot == 'hist'", ns = ns,
                  numericInput(ns('bins'), 'Bins', 10, 5, step = 10))
                ),
                column(3, div(insert_output_ui(ns('insert_dist_plot'))),
                    style = 'margin-top: 28px')
              ),
              div(style = "margin-bottom: -8px !important;"),
            )
          ),
          nav_panel(
            'Scatter',
            full_screen = T,
            card_body(plotOutput(ns('scatter_plot'))),
            card_footer(
              layout_column_wrap(
                checkboxInput(
                  ns('scatter_lm'),
                  list('Plot Linear Model', bs_icon('info-circle')) |>
                    ttip('Show the line only if LM model was created')),
                btn_task(ns('btn_scatter'), 'Generate Plot', icon('chart-simple')),
                insert_output_ui(ns('insert_scatter'))
              ),
              div(style = "margin-bottom: -18px !important;"),
            )
          ),
          nav_panel(
            'Table',
            full_screen = T,
            card_body(
              fluidRow(
                column(4,
                  radioGroupButtons(
                    ns('table_var'),
                    'Table variables:',
                    c('1 Variable' = '1v', '2 Variables' = '2v'),
                    size = 'sm',
                    individual = T
                  )
                ),
                column(4,
                  radioGroupButtons(
                    ns('table_type'),
                    'Table type:',
                    c('Absolute Values' = 'abs_table', 'Percent Values' = 'perc_table'),
                    size = 'sm',
                    individual = T
                  )
                ),
              ),
              gt_output(ns('table')),
            ),
            card_footer(insert_output_ui(ns('insert_table_values')))
          ),
          nav_panel(
            'Linear Model',
            full_screen = T,
            navset_card_pill(
              nav_panel(
                'Parameters',
                sliderInput(ns('sample_size'), 'Sample Size (%)', 0, 100, 100) |>
                  tooltip('Applied only if valid values are greater than 10.000'),
                layout_column_wrap(
                  btn_task(ns('btn_scatter_lm_run'), 'Run Linear Model', icon('gear')),
                  btn_task(ns('btn_scatter_lm_clear'), 'Clear Linear Model', icon('trash-can'))
                )
              ),
              nav_panel(
                'Output',
                card_body(gt_output(ns('lm_var_table')), gt_output(ns('lm_metrics'))),
                card_footer(insert_output_ui(ns('insert_lm_model_output')))
              ),
              nav_panel(
                'Residuals',
                plotOutput(ns('lm_resid_plot')),
                card_footer(
                  layout_column_wrap(
                    radioGroupButtons(ns('radio_lm_resid'), 'Plot type:',
                                 c('Histogram' = 'hist', 'Boxplot' = 'boxplot',
                                   'Dots' = 'dots'), size = 'sm', individual = T),
                    btn_task(ns('btn_lm_resid'), 'Plot residuals', icon('chart-simple'),
                             style = 'margin-top: 28px'),
                    div(insert_output_ui(ns('insert_lm_resid_plot')),
                        style = 'margin-top: 28px')
                  ),
                  div(style = "margin-bottom: -24px !important;"),
                )
              ),
            )
          ),
        ),
        navset_card_pill(
          nav_panel('Stats', full_screen = T, stats_table_ui(ns('pA_stats')),
                    card_footer(insert_output_ui(ns('insert_stats_table')))
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
exploratory_server <- function(id, output_report) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # df active ---------------------------------------------------------------
    df <- reactiveValues()
    observe({
      df$df_active <- get_act_dt(session)
    })

    var_analysis <- reactive({
      session$userData$dt$act_meta() |> filter(perc_nas != 1) |>  pull(var)
    })

    output$ui_var_names <- renderUI(
      selectInput(ns('sel_vars'),
                  list('Main Variable', bs_icon('info-circle')) |>
                    ttip('Dependent Variable'),
                    var_analysis())
    )

    output$ui_var_names2 <- renderUI(
      selectInput(ns('sel_vars2'),
                  list('Variable 2', bs_icon('info-circle')) |>
                         ttip('Independent Variable'),
                  var_analysis(), var_analysis()[2])
    )

    outliers_index <- reactive({
      v <- df$df_active[[input$sel_vars]]
      if(input$outliers & is.numeric(v)) {
        q1 <- p25(v)
        q3 <- p75(v)
        dist_interquatile <- q3 - q1
        v >= (q1 - 1.5 * dist_interquatile) & v <= (q3 + 1.5 * dist_interquatile)
      } else {
        rep(T, length(v))
      }
    })

    # values to analysis page -------------------------------------------------
    var <- reactive({
      req(input$sel_vars)
      df$df_active[[input$sel_vars]][outliers_index()]
    })

    var2 <- reactive({
      req(input$sel_vars2)
      df$df_active[[input$sel_vars2]][outliers_index()]
    })

    var_percentile <- reactive(
      if(isTruthy(input$var_percentile) && is.numeric(var()) &&
         between(input$var_percentile, 0, 100)){
        pn(var(), input$var_percentile / 100)
      } else { NA }
    )

    # render plots ------------------------------------------------------------
    dist_plot <- reactive({
      req(var())
      req(var2())

      if (input$radio_dist_plot == 'barplot'){
        validate(need(!is.numeric(var()), 'Var can not be numeric'))

        ggplot(data.frame(x = var()), aes(x = factor(x))) +
          geom_bar(fill = session$userData$conf$plot_fill_color) +
          labs(x = '', y = 'Count') +
          theme_classic() +
          theme(axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.y = element_text(size = 16)
          )

      } else {
        validate(need(is.numeric(var()), 'Var must be numeric'))

        if (input$radio_dist_plot == 'boxplot_group'){
          validate(
            need(!is.numeric(var2()) | (is.numeric(var2()) & is.integer(var2())),
                 'Variable 2 can not be float'),
            need(!is.complex(var2()), 'Variable 2 can not be complex')
          )

          ggplot(data.frame(x = {
            if (var2() |> is.numeric())
              as.factor(var2())
            else
              var2()
          }, y = var()), aes(x = x, y = y, fill = x)) +
            stat_boxplot(geom = 'errorbar', width = 0.3) +
            geom_boxplot(orientation = 'x') +
            geom_hline(yintercept = var_percentile(),
                       color = session$userData$conf$plot_line_color) +
            coord_flip() +
            labs(x = '', y = '') +
            theme_classic() +
            theme(
              legend.position = 'none',
              axis.ticks.y = element_blank(),
              axis.line.y  = element_blank(),
              panel.border = element_rect(color = 'black', fill = NA),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14)
            )

        } else {
          validate(
            need(isTruthy(input$var_percentile)
                 && between(input$var_percentile, 0, 100), 'Percentile must be between 0 and 100')
          )

          if(input$radio_dist_plot == 'hist'){
            validate(need(input$bins > 0, 'Bins must be 1 or higher'))

            ggplot(data.frame(x = var()), aes(x = x)) +
              geom_histogram(
                bins = input$bins,
                fill = session$userData$conf$plot_fill_color,
                color = '#000000'
              ) +
              geom_vline(xintercept = var_percentile(), color = session$userData$conf$plot_line_color) +
              labs(x = '', y = 'Count', title = '') +
              theme_classic() +
              theme(axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    axis.title.y = element_text(size = 16)
              )

          } else if (input$radio_dist_plot == 'boxplot'){
            ggplot(data = data.frame(x = var()), aes(x = x)) +
              stat_boxplot(geom = 'errorbar', width = 0.3) +
              geom_boxplot(fill = session$userData$conf$plot_fill_color) +
              ylim(-1.2, 1.2) +
              geom_vline(xintercept = var_percentile(), color = session$userData$conf$plot_line_color) +
              labs(x = '', y = '') +
              theme_classic() +
              theme(
                axis.ticks.y = element_blank(),
                axis.text.y  = element_blank(),
                axis.line.y  = element_blank(),
                panel.border = element_rect(color = '#000000', fill = NA),
                axis.text.x = element_text(size = 14)
              )

          } else if (input$radio_dist_plot == 'dots'){
            point_shape <- if(length(var()) > 1e4) '.' else 20

            ggplot(data = data.frame(x = seq_along(var()),
                                     y = var()), aes(x = x, y = y)) +
              geom_point(shape = point_shape, color = session$userData$conf$plot_fill_color) +
              geom_hline(yintercept = var_percentile(), color = session$userData$conf$plot_line_color) +
              labs(x = 'Index', y = 'Values') +
              theme_classic() +
              theme(axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    axis.title.x = element_text(size = 16),
                    axis.title.y = element_text(size = 16)
              )
          }
        }
      }
    })

    output$dist_plot <- renderPlot({
      req(dist_plot())
      dist_plot()
    }, res = 96)
    # render scatter plot -----------------------------------------------------
    scatter_plot <- reactive({
      point_shape <- if(length(var()) > 1e4) "." else 20

      if (input$scatter_lm &&
          linear_model$y_name == input$sel_vars &&
          linear_model$x_name == input$sel_vars2) {

        ggplot(data.frame(x = var2(), y = var()), aes(x = x, y = y)) +
          geom_point(color = session$userData$conf$plot_fill_color, shape = point_shape) +
          geom_line(
            data = data.frame(x = linear_model$x, y = linear_model$y),
            aes(x = x, y = y),
            color = session$userData$conf$plot_line_color,
            linewidth = 1
          ) +
          labs(
            title = paste(
              'Adjusted R Squared:',
              summary(linear_model$model)$r.squared |> round(4)
            ),
            x = input$sel_vars2,
            y = input$sel_vars
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16)
          )
      } else {
        ggplot(data.frame(x = var2(), y = var()), aes(x = x, y = y)) +
          geom_point(color = session$userData$conf$plot_fill_color, shape = point_shape) +
          labs(title = paste('Pearson Correlation:', stats_correlation() |> round(4)),
               x = input$sel_vars2, y = input$sel_vars) +
          theme_classic() +
          theme(axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16)
          )
      }
    })|> bindEvent(input$btn_scatter)

    output$scatter_plot <- renderPlot({
      validate(
        need(is.numeric(var()) && is.numeric(var2()), 'Variables must be numeric')
      )
      scatter_plot()
    }, res = 96)

    # tables ------------------------------------------------------------------
    table_values <- reactive({
      req(var())

      if(input$table_var == '1v') {

        validate(need(is.character(var()) || is.factor(var()) || is.logical(var()),
                      'Var must be character, factor or logical'))

        if(input$table_type == 'abs_table'){
          tab1 <- var() |> table()
        } else if(input$table_type == 'perc_table'){
          tab1 <- var() |> table() |> prop.table() * 100
        }

        tab1 |> as.data.frame()

      } else if (input$table_var == '2v'){
        req(var())
        req(var2())

        validate(need(
          input$sel_vars != input$sel_vars2 &
          (is.character(var()) || is.factor(var()) || is.logical(var())) &
            (is.character(var2()) || is.factor(var2()) || is.logical(var2())),
          'Select two diferent variables of type character, factor or logical'))

        if(input$table_type == 'abs_table'){
          tab1 <- table(var(), var2())
        } else if(input$table_type == 'perc_table'){
          tab1 <- table(var(), var2()) |> prop.table() * 100
        }

        tab1 <- tab1 |> as.data.frame.matrix()

        cbind(var1 = rownames(tab1), tab1)
      }
    })

    table_values_gt <- reactive({
      req(table_values())
      req(input$table_var, input$sel_vars, input$sel_vars2)

      if(input$table_var == '1v') {

        if(input$table_type == 'abs_table'){
          y_label <- 'Frequency'
        } else if(input$table_type == 'perc_table'){
          y_label <- 'Relative Frequency (%)'
        }

        table_values() |>
          gt() |>
          cols_label(
            Var1 = input$sel_vars,
            Freq = y_label
          )
      } else if (input$table_var == '2v'){

        if(input$table_type == 'abs_table'){
          y_label <- input$sel_vars2
        } else if(input$table_type == 'perc_table'){
          y_label <- paste(input$sel_vars2, '(%)')
        }

        table_values() |>
          gt() |>
            cols_label(var1 = "") |>
            tab_spanner(
              label = input$sel_vars,
              columns = var1
            ) |>
            tab_spanner(
              label = y_label,
              columns = names(table_values())[2:length(names(table_values()))]
            )
      }
    })

    output$table <- render_gt({
      req(table_values_gt())
      table_values_gt() |>
        opt_interactive()
    })

    # linear model ------------------------------------------------------------
    linear_model <- reactiveValues(
      model = NULL,
      x = NULL,
      y = NULL,
      x_name = '',
      y_name = ''
    )

    observe({
      if(!is.numeric(var())){
        msg('The Dependent variable must be numeric', 2.5)
      } else if (input$sel_vars == input$sel_vars2) {
        msg('Choose diferent variables for X and Y.', 2.5)
      } else {
        linear_model$y_name <- input$sel_vars
        linear_model$x_name <- input$sel_vars2

        var_size <- length(var())

        if(var_size < 10e3) {
          var_y <- var()
          var_x <- var2()
        } else {
          sample_size <- min(var_size,
                                  floor(var_size * min(1, max(0, input$sample_size/100))))
          lm_sample <- sample.int(var_size, sample_size, replace = F) |>
            sort()
          var_y <- var()[lm_sample]
          var_x <- var2()[lm_sample]
        }

        linear_model$model <- lm(var_y ~ var_x, model = F)
        linear_model$x <- var_x
        linear_model$y <- linear_model$model$fitted.values
        msg('Lm model completed.')
      }
    }) |> bindEvent(input$btn_scatter_lm_run)

    observe({
      linear_model$model <- NULL
      linear_model$model$residuals <- NULL
      linear_model$x <- NULL
      linear_model$y <- NULL
      linear_model$x_name <- ''
      linear_model$y_name <- ''

      # clear the residual plot to avoid incorrect output element
      update_lm_resid_plot(update_lm_resid_plot() + 1)

      msg('Lm model cleared.')
    }) |> bindEvent(input$btn_scatter_lm_clear)

    # print linear model ------------------------------------------------------
    lm_var_table <- reactive({
      req(linear_model$model)
      output <- linear_model_df_output(linear_model$model |> summary())
      output$Variable <- gsub('var_x', linear_model$x_name, output$Variable)

      output |>
        gt() |>
        tab_header(title = 'Linear Model',
                   subtitle = paste('Independent Variable:',
                                    linear_model$y_name))
    })

    lm_metrics <- reactive({
      req(linear_model$model)
      linear_model_df_metrics(linear_model$model |> summary()) |>
        gt() |> tab_header('Model metrics')
    })

    output$lm_var_table <- render_gt({
      req(lm_var_table())
      lm_var_table()
    })

    output$lm_metrics <- render_gt({
      req(lm_metrics())
      lm_metrics()
    })

    # plot linear model residuals ---------------------------------------------
    update_lm_resid_plot <- reactiveVal(0)

    observe({
      update_lm_resid_plot(update_lm_resid_plot() + 1)
    }) |> bindEvent(input$btn_lm_resid)

    lm_resid_plot <- reactive({
      req(linear_model$model$residuals)

      if(input$radio_lm_resid == 'hist'){
        ggplot(data = data.frame(x = linear_model$model$residuals), aes(x = x)) +
          geom_histogram(bins = 10,
                         fill = session$userData$conf$plot_fill_color,
                         color = '#000000') +
          labs(x = '', y = 'Count', title = '') +
          theme_classic() +
          theme(axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.y = element_text(size = 16)
          )

      } else if (input$radio_lm_resid == 'boxplot'){
        ggplot(data = data.frame(x = linear_model$model$residuals), aes(x = x)) +
          stat_boxplot(geom = 'errorbar', width = 0.3) +
          geom_boxplot(fill = session$userData$conf$plot_fill_color) +
          ylim(-1.2, 1.2) +
          labs(x = '', y = '') +
          theme_classic() +
          theme(
            axis.ticks.y = element_blank(),
            axis.text.y  = element_blank(),
            axis.line.y  = element_blank(),
            panel.border = element_rect(color = '#000000', fill = NA),
            axis.text.x = element_text(size = 14)
          )

      } else if (input$radio_lm_resid == 'dots'){
        point_shape <- if(length(linear_model$model$residuals) > 1e4) '.' else 20

        ggplot(data = data.frame(x = seq_along(linear_model$model$residuals),
                                 y = linear_model$model$residuals),
               aes(x = x, y = y)) +
          geom_point(shape = point_shape,
                     color = session$userData$conf$plot_fill_color) +
          geom_hline(yintercept = 0,
                     color = session$userData$conf$plot_line_color,
                     linetype = 2) +
          labs(x = 'Index', y = 'Values') +
          theme_classic() +
          theme(axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16)
          )
      }
    }) |> bindEvent(update_lm_resid_plot())

    output$lm_resid_plot <- renderPlot({
      validate(need(isTruthy(linear_model$model), 'No residuals to plot'))

      lm_resid_plot()
    }, res = 96)

    # metrics -----------------------------------------------------------------
    stats_sd <- reactive(if(is.numeric(var())) sd(var(), na.rm = T) else NA)

    stats_correlation <- reactive(
      if(is.numeric(var()) && is.numeric(var2()) && stats_sd() != 0 &&
         !is.na(stats_sd())){
        sd_var2 <- sd(var2(), na.rm = T)
        if(sd_var2 == 0 || sd_var2 |> is.na()) {
          NA
        } else {
          cor(var(), var2(), method = 'p', use = 'na.or.complete')
        }
      } else { NA }
    )
    # stats table -------------------------------------------------------------
    mod_stats_table <- stats_table_server(
      'pA_stats',
      var,
      var2,
      reactive(input$var_percentile),
      var_percentile,
      stats_sd,
      stats_correlation
    )

    # get return from stats table ---------------------------------------------
    stats_table <- reactive({
      req(mod_stats_table$table())
      mod_stats_table$table()
    })

    # insert dist plot to output ----------------------------------------------
    mod_output_dist_plot <- insert_output_server(
      'insert_dist_plot',
      reactive(plotTag(dist_plot(), '', width = 1000, height = 500)))

    # get return from insert output module ------------------------------------
    observe({
      req(mod_output_dist_plot$output_element())
      req(mod_output_dist_plot$output_element()$id)

      session$userData$out$elements[[mod_output_dist_plot$output_element()$id]] <- mod_output_dist_plot$output_element()

    }) |> bindEvent(mod_output_dist_plot$output_element())

    # insert scatter to output ------------------------------------------------
    mod_output_scatter <- insert_output_server(
      'insert_scatter',
      reactive(plotTag(scatter_plot(), '', width = 1000, height = 500)))

    # get return from insert output module ------------------------------------
    observe({
      req(mod_output_scatter$output_element())
      req(mod_output_scatter$output_element()$id)

      session$userData$out$elements[[mod_output_scatter$output_element()$id]] <- mod_output_scatter$output_element()

    }) |> bindEvent(mod_output_scatter$output_element())

    # insert lm model output --------------------------------------------------
    mod_output_lm_model_output <- insert_output_server(
      'insert_lm_model_output',
      reactive(gen_table2(lm_var_table(), lm_metrics()))
    )

    # get return from insert output module ------------------------------------
    observe({
      req(mod_output_lm_model_output$output_element())
      req(mod_output_lm_model_output$output_element()$id)

      session$userData$out$elements[[mod_output_lm_model_output$output_element()$id]] <- mod_output_lm_model_output$output_element()

    }) |> bindEvent(mod_output_lm_model_output$output_element())

    # insert lm residual plot to output ---------------------------------------
    mod_output_lm_resid_plot <- insert_output_server(
      'insert_lm_resid_plot',
      reactive(plotTag(lm_resid_plot(), '', width = 1000, height = 500)))

    # get return from insert output module ------------------------------------
    observe({
      req(mod_output_lm_resid_plot$output_element())
      req(mod_output_lm_resid_plot$output_element()$id)

      session$userData$out$elements[[mod_output_lm_resid_plot$output_element()$id]] <- mod_output_lm_resid_plot$output_element()

    }) |> bindEvent(mod_output_lm_resid_plot$output_element())

    # insert stats table ------------------------------------------------------
    mod_output_stats_table <- insert_output_server(
      'insert_stats_table', stats_table)

    # get return from insert output module ------------------------------------
    observe({
      req(mod_output_stats_table$output_element())
      req(mod_output_stats_table$output_element()$id)

      session$userData$out$elements[[mod_output_stats_table$output_element()$id]] <- mod_output_stats_table$output_element()

    }) |> bindEvent(mod_output_stats_table$output_element())

    # insert table of values to output ----------------------------------------
    mod_output_table_values <- insert_output_server(
      'insert_table_values',
      table_values_gt
    )

    # get return from insert output module ------------------------------------
    observe({
      req(mod_output_table_values$output_element())
      req(mod_output_table_values$output_element()$id)

      session$userData$out$elements[[mod_output_table_values$output_element()$id]] <- mod_output_table_values$output_element()

    }) |> bindEvent(mod_output_table_values$output_element())

  })
}
