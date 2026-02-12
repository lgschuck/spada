
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

      var_analysis <- session$userData$dt$act_meta() |> filter(perc_nas != 1) |> pull(var)

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

    norm_hist <- reactive({
      req(df_active())
      req(input$sel_var)
      req(var())

      spada_plot(type = 'hist_density',
                 df = data.frame(x = var()),
                 xvar = 'x',
                 xlab = 'Values',
                 ylab = 'Density',
                 title = paste('Histogram -', input$sel_var),
                 bins = input$bins,
                 fill_color = session$userData$conf$plot_fill_color,
                 line_color = session$userData$conf$plot_line_color,
                 sample_limit = session$userData$conf$plot_limit,
                 mean_value = fmean(var(), na.rm = TRUE),
                 sd_value = fsd(var(), na.rm = TRUE)
      )
    }) |> bindEvent(input$btn_hist)

    # insert histogram to output ----------------------------------------------
    insert_output_server(
      'norm_insert_output_hist',
      reactive(
        plotTag(norm_hist(), '', width = 1000, height = 500)
      ),
      'Histogram'
    )

    # qq plot -----------------------------------------------------------------
    output$qq_plot <- renderPlot({
      norm_qq_plot()
    }, res = 96)

    norm_qq_plot <- reactive({
      req(input$sel_var)
      req(var())

      spada_plot(type = 'qq_plot',
                 df = data.frame(x = var()),
                 xvar = 'x',
                 xlab = 'Theoretical Quantiles',
                 ylab = 'Sample Quantiles',
                 title = paste('Normal QQ Plot:', input$sel_var),
                 fill_color = session$userData$conf$plot_fill_color,
                 line_color = session$userData$conf$plot_line_color,
                 sample_limit = session$userData$conf$plot_limit
      )
    }) |> bindEvent(input$btn_qq)

    # insert to output --------------------------------------------------------
    insert_output_server(
      'norm_insert_output_qq',
      reactive(plotTag(norm_qq_plot(), '', width = 1000, height = 500)),
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
        inputed_error <- reactive(abs(mina(var())/1e6))

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
      tagList(
        stati_card(ks_results()$results |>
                   filter(results %in% c('statistic.D')) |>
                   pull(values) |>
                   as.numeric() |>
                   f_num(dig = 5),
                   'Statistic D (test value)'),
        stati_card(ks_results()$results |>
                   filter(results %in% c('p.value')) |>
                   pull(values) |>
                   as.numeric() |>
                   f_num(dig = 5),
                   'p value')
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
      showModal(modalDialog(
        HTML(get_help_file('stats', 'ks.test')),
        easyClose = TRUE, size = 'xl'
      ))
    }) |> bindEvent(input$btn_help_ks)

    # sw test -----------------------------------------------------------------
    sw_results <- reactive({
      req(input$sel_var)
      req(var())
      req(var_len())

      if (var_len() < 3 || var_len() > 5000) {
        msg(paste0('Sample size must be between 3 and 5000 (actual: ', var_len(), ')'), 3)

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
        gt::cols_label('values' = 'Values', 'results' = 'Results') |>
        tab_header('Shapiro-Wilk Normality Test')
    })

    # sharpiro-wilk staticards ------------------------------------------------
    output$conditional_staticard_sw <- renderUI({
      req(sw_results())
      tagList(
        stati_card(sw_results() |>
                   filter(results %in% c('statistic.W')) |>
                   pull(values) |>
                   as.numeric() |>
                   f_num(dig = 5),
                   'Statistic W (test value)'),
        stati_card(sw_results() |>
                   filter(results %in% c('p.value')) |>
                   pull(values) |>
                   as.numeric() |>
                   f_num(dig = 5),
                   'p value')
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
      showModal(modalDialog(
        HTML(get_help_file('stats', 'shapiro.test')),
        easyClose = TRUE, size = 'xl'
      ))
    }) |> bindEvent(input$btn_help_sw)

    # sf test -----------------------------------------------------------------
    sf_results <- reactive({
      req(input$sel_var)
      req(var())
      req(var_len())

      if (var_len() < 5 || var_len() > 5000) {
        msg(paste0('Sample size must be between 5 and 5000 (actual: ', var_len(), ')'), 3)

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
        gt::cols_label('values' = 'Values', 'results' = 'Results') |>
        tab_header('Shapiro-Francia Normality Test')
    })


    # sharpiro-francia staticards ---------------------------------------------
    output$conditional_staticard_sf <- renderUI({
      req(sf_results())
      tagList(
        stati_card(sf_results() |>
                     filter(results %in% c('statistic.W')) |>
                     pull(values) |>
                     as.numeric() |>
                     f_num(dig = 5),
                   'Statistic W (test value)'),
        stati_card(sf_results() |>
                     filter(results %in% c('p.value')) |>
                     pull(values) |>
                     as.numeric() |>
                     f_num(dig = 5),
                   'p value')
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
      showModal(modalDialog(
        HTML(get_help_file('DescTools', 'ShapiroFranciaTest')),
        easyClose = TRUE, size = 'xl'
      ))
    }) |> bindEvent(input$btn_help_sf)

  })
}
