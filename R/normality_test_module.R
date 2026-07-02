
# ui --------------------------------------------------------------------------
normality_test_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Normality Test', class = 'mini-header'),
    layout_sidebar(
      class = 'card-sidebar',
      sidebar = sidebar(uiOutput(ns('parameters'))),
      navset_card_pill(
        nav_panel(
          'Histogram',
          card(
            full_screen = T,
            card_body(plotOutput(ns('hist'))),
            card_footer(
              div(style = "margin-bottom: -8px !important;"),
              layout_columns(
                col_widths = c(1, 2, 2),
                numericInput(ns('bins'), 'Bins', 25, 5, step = 5),
                btn_task(ns('btn_hist'), 'Show Histogram', icon('chart-simple'),
                         style = 'margin-top: 28px'),
                div(insert_output_ui(ns('norm_insert_output_hist')),
                    style = 'margin-top: 28px')
              ),
              div(style = "margin-bottom: -24px !important;"),
            )
          )
        ),
        nav_panel(
          'QQ Plot',
          card(
            full_screen = T,
            card_body(plotOutput(ns('qq_plot'))),
            card_footer(
              btn_task(ns('btn_qq'), 'Show QQ plot', icon('chart-simple')),
              insert_output_ui(ns('norm_insert_output_qq'))
            )
          )
        ),
        nav_panel(
          'KS Test',
          card(
            full_screen = T,
            card_body(
              layout_columns(
                col_widths = c(3, 7, 2),
                uiOutput(ns('conditional_staticard_ks')),
                gt_output(ns('ks_test')),
                div(
                  uiOutput(ns('conditional_save_ks_gt')),
                  br(), br(),
                  uiOutput(ns('conditional_add_output_ks'))
                )
              ),
              uiOutput(ns('ks_test_obs_ui'))
            ),
            card_footer(
              btn_task(ns('btn_ks'), 'Run Test', icon('gear')),
              btn_task(ns('btn_help_ks'), 'Help', icon('question'))
            )
          )
        ),
        nav_panel(
          'Shapiro-Wilk Test',
          card(
            full_screen = T,
            card_body(
              layout_columns(
                col_widths = c(3, 7, 2),
                uiOutput(ns('conditional_staticard_sw')),
                gt_output(ns('sw_test')),
                div(
                  uiOutput(ns('conditional_save_sw_gt')),
                  br(), br(),
                  uiOutput(ns('conditional_add_output_sw'))
                )
              )
            ),
            card_footer(
              btn_task(ns('btn_sw'), 'Run Test', icon('gear')),
              btn_task(ns('btn_help_sw'), 'Help', icon('question'))
            )
          )
        ),
        nav_panel(
          'Shapiro-Francia Test',
          card(
            full_screen = T,
            card_body(
              layout_columns(
                col_widths = c(3, 7, 2),
                uiOutput(ns('conditional_staticard_sf')),
                gt_output(ns('sf_test')),
                div(
                  uiOutput(ns('conditional_save_sf_gt')),
                  br(), br(),
                  uiOutput(ns('conditional_add_output_sf'))
                )
              )
            ),
            card_footer(
              btn_task(ns('btn_sf'), 'Run Test', icon('gear')),
              btn_task(ns('btn_help_sf'), 'Help', icon('question'))
            )
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
normality_test_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  df <- reactive(get_act_dt(session))

    df_active <- reactive({
      req(df())
      df()[, lapply(df(), is.numeric) == T, with = F]
    })

    var_analysis <- reactive({
      req(df_active())

      df_names <- df_active() |> names()

      var_analysis <- session$userData$dt$act_meta()[perc_nas != 1, var]

      df_names[df_names %in% var_analysis]
    })

    var <- reactive({
      req(df_active())
      req(input$sel_var)

      temp <- df_active()[[input$sel_var]]
      temp[!is.na(temp)]
    })

    var_len <- reactive(var() |> length())

    output$parameters <- renderUI({
      req(var_analysis())
      tagList(
        selectInput(ns('sel_var'), 'Variable 1', var_analysis()),
        p('* Showing only numeric variables')
      )
    })

    # histogram ---------------------------------------------------------------
    output$hist <- renderPlot({
      validate(need(input$bins > 0, 'Bins must be 1 or higher'))

      norm_hist()
    }, res = 96)

    task_hist <- ExtendedTask$new(function(df,
                                           title,
                                           bins,
                                           mean_value,
                                           sd_value,
                                           plot_conf) {

      mirai({
        spada_plot(
          type = 'hist_density',
          df = df,
          xvar = 'x',
          xlab = 'Values',
          ylab = 'Density',
          title = title,
          bins = bins,
          point_shape = if(plot_conf$plot_limit > 1e4 && nrow(df) > 1e4) '.' else 20,
          mean_value = mean_value,
          sd_value = sd_value,
          plot_conf = plot_conf
        )
      },
      df = df,
      title = title,
      bins = bins,
      mean_value = mean_value,
      sd_value = sd_value,
      plot_conf = plot_conf)
    }) |> bind_task_button('btn_hist')

    observe({
      req(input$sel_var, var())
      task_hist$invoke(
        df = data.frame(x = var()),
        bins = input$bins,
        title = paste('Histogram -', input$sel_var),
        mean_value = fmean(var(), na.rm = TRUE),
        sd_value = fsd(var(), na.rm = TRUE),
        plot_conf = reactiveValuesToList(session$userData$conf)
      )
    }) |> bindEvent(input$btn_hist)

    norm_hist <- reactive({ task_hist$result() })

    # insert histogram to output ----------------------------------------------
    insert_output_server(
      'norm_insert_output_hist',
      reactive(plot_tag(norm_hist())),
      'Histogram'
    )

    # qq plot -----------------------------------------------------------------
    output$qq_plot <- renderPlot({
      norm_qq_plot()
    }, res = 96)

    task_qq <- ExtendedTask$new(function(df,
                                         title,
                                         plot_conf) {
      mirai({
        spada_plot(
          type = 'qq_plot',
          df = df,
          xvar = 'x',
          xlab = 'Theoretical Quantiles',
          ylab = 'Sample Quantiles',
          title = title,
          plot_conf = plot_conf
        )
      },
      df = df,
      title = title,
      plot_conf = plot_conf)
    }) |> bind_task_button('btn_qq')

    observe({
      req(input$sel_var, var())
      task_qq$invoke(
        df = data.frame(x = var()),
        title = paste('Normal QQ Plot:', input$sel_var),
        plot_conf = reactiveValuesToList(session$userData$conf)
      )
    }) |> bindEvent(input$btn_qq)

    norm_qq_plot <- reactive({ task_qq$result() })

    # insert to output --------------------------------------------------------
    insert_output_server(
      'norm_insert_output_qq',
      reactive(plot_tag(norm_qq_plot())),
      'QQ Plot'
    )

    output$conditional_add_output_qq <- renderUI({
      req(norm_qq_plot())
      insert_output_ui(ns('norm_insert_output_qq'))
    })

    # ks test -----------------------------------------------------------------
    ks_results <- reactive({
      req(input$sel_var)
      req(var())

      if(anyDuplicated(var()) > 0){
        # small error to avoid ties
        inputed_error <- reactive(abs(min_nona(var())/1e6))

        test_value <- reactive(var() + rnorm(length(var()),
                                             mean = 0,
                                             sd = inputed_error()))

        df <- ks.test(test_value(), 'pnorm') |> unlist() |> as.data.frame()

        df$results <- rownames(df)
        names(df) <- c('values', 'results')

        df[df$results == 'data.name', ]$values <- paste(input$sel_var)

        results = list(
          'results' = df,
          'observations' = paste0('There are tied values, normally distributed',
                                  ' random noise was added (mean = 0 and sd = ',
                                  inputed_error(), ')')
        )
      } else {
        df <- ks.test(var(), 'pnorm') |> unlist() |> as.data.frame()

        df$results <- rownames(df)
        names(df) <- c('values', 'results')

        df[df$results == 'data.name', ]$values <- paste(input$sel_var)

        results = list('results' = df)
      }

    }) |> bindEvent(input$btn_ks)

    ks_results_gt <- reactive({
      req(ks_results())

      ks_results()$results |>
        gt() |>
        cols_move(columns = 'values', after = 'results') |>
        gt::cols_label('values' = 'Values', 'results' = 'Results') |>
        tab_header('Kolmogorov-Smirnov Normality Test')
    })

    # ks staticards -----------------------------------------------------------
    output$conditional_staticard_ks <- renderUI({
      req(ks_results())

      stat_d <- ks_results()$results$values[which(ks_results()$results$results == 'statistic.D')]
      p_value <- ks_results()$results$values[which(ks_results()$results$results == 'p.value')]

      tagList(
        stati_card(stat_d |> as.numeric() |> f_num(dig = 5), 'Statistic D (test value)'),
        stati_card(p_value |> as.numeric() |> f_num(dig = 5), 'p value')
      )
    })

    output$ks_test <- render_gt({
      req(ks_results_gt())
      ks_results_gt()
    })

    output$ks_test_obs <- renderPrint({
      req(ks_results()$observations)
      ks_results()$observations
    })

    output$ks_test_obs_ui <- renderUI({
      req(ks_results()$observations)
      verbatimTextOutput(ns('ks_test_obs'))
    })

    save_gt_server('ks_save_gt', ks_results_gt)

    output$conditional_save_ks_gt <- renderUI({
      req(ks_results_gt())
      save_gt_ui(ns('ks_save_gt'))
    })

    # insert to output --------------------------------------------------------
    insert_output_server(
      'norm_insert_output_ks',
      reactive(ks_results_gt()),
      'Kolmogorov-Smirnov Normality Test'
    )

    output$conditional_add_output_ks <- renderUI({
      req(ks_results_gt())
      insert_output_ui(ns('norm_insert_output_ks'))
    })

    # help ks function --------------------------------------------------------
    observe({
      fun_help_modal('stats', 'ks.test')
    }) |> bindEvent(input$btn_help_ks)

    # sw test -----------------------------------------------------------------
    sw_results <- reactive({
      req(input$sel_var)
      req(var())
      req(var_len())

      var_len <- var_len()
      if (var_len < 3 || var_len > 5000) {
        msg(paste0('Sample size must be between 3 and 5000 (actual: ', var_len, ')'), 3)

        return()
      }

      if (test_all_equal(var())) {
        msg('Shapiro-Wilk test: the values can not be all equal')
        return()
      }

      df <- shapiro.test(var()) |> unlist() |> as.data.frame()

      df$results <- rownames(df)
      names(df) <- c('values', 'results')

      df[df$results == 'data.name', ]$values <- paste(input$sel_var)

      df
    }) |> bindEvent(input$btn_sw)

    sw_results_gt <- reactive({
      req(sw_results())

      sw_results() |>
        gt() |>
        cols_move(columns = 'values', after = 'results') |>
        cols_label('values' = 'Values', 'results' = 'Results') |>
        tab_header('Shapiro-Wilk Normality Test')
    })

    # sharpiro-wilk staticards ------------------------------------------------
    output$conditional_staticard_sw <- renderUI({
      req(sw_results())

      stat_w <- sw_results()$values[which(sw_results()$results == 'statistic.W')]
      p_value <- sw_results()$values[which(sw_results()$results == 'p.value')]

      tagList(
        stati_card(stat_w |> as.numeric() |> f_num(dig = 5), 'Statistic W (test value)'),
        stati_card(p_value |> as.numeric() |> f_num(dig = 5), 'p value')
      )
    })

    output$sw_test <- render_gt({
      req(sw_results_gt())
      sw_results_gt()
    })

    save_gt_server('sw_save_gt', sw_results_gt)

    output$conditional_save_sw_gt <- renderUI({
      req(sw_results_gt())
      save_gt_ui(ns('sw_save_gt'))
    })

    # insert to output --------------------------------------------------------
    insert_output_server(
      'norm_insert_output_sw',
      reactive(sw_results_gt()),
      'Shapiro-Wilk Normality Test'
    )

    output$conditional_add_output_sw <- renderUI({
      req(sw_results_gt())
      insert_output_ui(ns('norm_insert_output_sw'))
    })

    # help file of shapiro.test
    observe({
      fun_help_modal('stats', 'shapiro.test')
    }) |> bindEvent(input$btn_help_sw)

    # sf test -----------------------------------------------------------------
    sf_results <- reactive({
      req(input$sel_var)
      req(var())
      req(var_len())
      var_len <- var_len()
      if (var_len < 5 || var_len > 5000) {
        msg(paste0('Sample size must be between 5 and 5000 (actual: ', var_len, ')'), 3)

        return()
      }

      if (test_all_equal(var())) {
        msg('Shapiro-Francia test: the values can not be all equal')
        return()
      }

      df <- ShapiroFranciaTest(var()) |> unlist() |> as.data.frame()

      df$results <- rownames(df)
      names(df) <- c('values', 'results')

      df[df$results == 'data.name', ]$values <- paste(input$sel_var)

      df
    }) |> bindEvent(input$btn_sf)

    sf_results_gt <- reactive({
      req(sf_results())

      sf_results() |>
        gt() |>
        cols_move(columns = 'values', after = 'results') |>
        cols_label('values' = 'Values', 'results' = 'Results') |>
        tab_header('Shapiro-Francia Normality Test')
    })


    # sharpiro-francia staticards ---------------------------------------------
    output$conditional_staticard_sf <- renderUI({
      req(sf_results())

      stat_w <- sf_results()$values[which(sw_results()$results == 'statistic.W')]
      p_value <- sf_results()$values[which(sw_results()$results == 'p.value')]

      tagList(
        stati_card(stat_w |> as.numeric() |> f_num(dig = 5), 'Statistic W (test value)'),
        stati_card(p_value |> as.numeric() |> f_num(dig = 5), 'p value')
      )
    })

    output$sf_test <- render_gt({
      req(sf_results_gt())
      sf_results_gt()
    })

    save_gt_server('sf_save_gt', sf_results_gt)

    output$conditional_save_sf_gt <- renderUI({
      req(sf_results_gt())
      save_gt_ui(ns('sf_save_gt'))
    })

    # insert to output --------------------------------------------------------
    insert_output_server(
      'norm_insert_output_sf',
      reactive(sf_results_gt()),
      'Shapiro-Francia Normality Test'
    )

    output$conditional_add_output_sf <- renderUI({
      req(sf_results_gt())
      insert_output_ui(ns('norm_insert_output_sf'))
    })

    # help file of ShapiroFranciaTest
    observe({
      fun_help_modal('DescTools', 'ShapiroFranciaTest')
    }) |> bindEvent(input$btn_help_sf)

  })
}
