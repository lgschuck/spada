
# ui --------------------------------------------------------------------------
z_test_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Z Test', class = 'mini-header'),
    layout_sidebar(
      class = 'card-sidebar',
      sidebar = sidebar(uiOutput(ns('parameters'))),
      navset_card_pill(
        nav_panel(
          'Test',
          card(
            layout_sidebar(
              sidebar = sidebar(
                h5('Sample Values', style = 'margin-bottom: -18px;'),
                div(textOutput(ns('sample_mean')), textOutput(ns('sample_sd'))),
                h5('Parameters', style = 'margin-bottom: -18px;'),
                numericInput(
                  ns('mu'),
                  list('Mean', bs_icon('info-circle') |>
                         ttip('Hypothesized Mean')), 0, width = '180px'),
                numericInput(
                  ns('sd'),
                  list('Std Deviation', bs_icon('info-circle') |>
                         ttip('Standard Deviation of Population')),
                  value = 1, min = 0, width = '180px'),
                radioButtons(ns('radio_alternative'), 'Alternative',
                             c('Two sided' = 'two.sided',
                               'Less' = 'less',
                               'Greater' = 'greater')),
                numericInput(ns('confidence'), 'Confidence Interval - %',
                             value = 95, 0, 100, 5, width = '180px'),
                layout_columns(
                  col_widths = c(8),
                  btn_task(ns('btn_run_test'), 'Run Test', icon('gear'))
                ),
                layout_columns(
                  col_widths = c(8),
                  btn_task(ns('btn_help_ztest'), 'Help', icon('question'))
                ),
              ),

              card_body(
                layout_columns(
                  col_widths = c(8, 4),
                  div(
                    uiOutput(ns('conditional_plot')),

                    br(),

                    uiOutput(ns('conditional_staticard_ztest'))
                  ),
                  div(
                    gt_output(ns('ztest_gt')),
                    br(),
                    uiOutput(ns('conditional_save_gt'))
                  )
                )
              ),
              card_footer(uiOutput(ns('conditional_add_output')))
            )
          )
        ),
        nav_panel(
          'Histogram',
          card(
            full_screen = T,
            card_body(plotOutput(ns('hist'))),
            card_footer(
              div(style = 'margin-bottom: -8px !important;'),
              layout_columns(
                col_widths = c(1, 3, 3),
                numericInput(ns('bins'), 'Bins', 25, 5, step = 5),
                btn_task(ns('btn_hist'), 'Show Histogram', icon('chart-simple'),
                         style = 'margin-top: 28px'),
                div(insert_output_ui(ns('ztest_insert_output_hist')),
                    style = 'margin-top: 28px')
              ),
              div(style = 'margin-bottom: -24px !important;'),
            )
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
z_test_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  df <- reactive(get_act_dt(session))

    ztest <- reactiveValues(results = NULL)

    df_active <- reactive(df()[, lapply(df(), is.numeric) == T, with = F])

    var_analysis <- reactive({
      df_names <- df_active() |> names()

      var_analysis <- session$userData$dt$act_meta()[perc_nas != 1, var]

      df_names[df_names %in% var_analysis]
    })

    var <- reactive({
      req(input$sel_var)
      df_active()[[input$sel_var]]
    })

    # calculate values --------------------------------------------------------
    sample_mean <- reactive(var() |> fmean(na.rm = T))

    output$sample_mean <- renderText({
      paste('Mean:', sample_mean() |> f_num(dig = 2))
    })

    sample_sd <- reactive(var() |> fsd(na.rm = T))

    output$sample_sd <- renderText({
      paste('Std Deviation:', sample_sd() |> f_num(dig = 2))
    })

    # input vars --------------------------------------------------------------
    output$parameters <- renderUI({
      tagList(
        selectInput(ns('sel_var'), 'Variable', var_analysis()),
        p('* Showing only numeric variables')
      )
    })

    # run z test --------------------------------------------------------------
    observe({
      req(input$sel_var)
      req(input$radio_alternative)

      if(!isTruthy(input$mu)){
        msg_error('Inform a value for the Mean')
      } else if(!isTruthy(input$sd)){
        msg_error('Inform a value for the Std Deviation')
      } else if(input$sd <= 0){
        msg_error('Standard Deviation must be positive ( > 0)', 2)
      } else if(!isTruthy(input$confidence) ||
         !between(input$confidence, 0, 100)) {
        msg_error('Confidence interval must be between 0 and 100%', 2)
      } else if (fnobs(var()) < 2) {
        msg_error('Inform at least 2 valid values')
      } else {
        df <- ZTest(var(),
                    alternative = input$radio_alternative,
                    mu = input$mu,
                    sd_pop = input$sd,
                    conf.level = input$confidence/100)

        df <- df |>
          unlist() |> as.data.frame()

        df$results <- rownames(df)
        names(df) <- c('values', 'results')

        df[df$results == 'data.name', ]$values <- input$sel_var

        ztest$results <- df

        ztest$confidence <- input$confidence/100
      }
    }) |> bindEvent(input$btn_run_test)

    # results gt table --------------------------------------------------------
    ztest_results_gt <- reactive({
      req(ztest$results)
      ztest$results |>
        gt() |>
        cols_move(columns = 'values', after = 'results') |>
        cols_label('values' = 'Values', 'results' = 'Results') |>
        tab_header('Z Test')
    })

    output$ztest_gt <- render_gt({
      req(ztest_results_gt())
      ztest_results_gt()
    })

    # save gt module ----------------------------------------------------------
    save_gt_server('ztest_save_gt', ztest_results_gt)

    output$conditional_save_gt <- renderUI({
      req(ztest_results_gt())
      save_gt_ui(ns('ztest_save_gt'))
    })

    # insert to output --------------------------------------------------------
    insert_output_server(
      'ztest_insert_output',
      reactive(
        gen_table2(
          plot_tag(ztest_plot()(), w = 600, h = 300),
          ztest_results_gt(),
          '75%',
          '25%'
        )
      ),
      'Z Test'
    )

    output$conditional_add_output <- renderUI({
      req(ztest_results_gt())
      insert_output_ui(ns('ztest_insert_output'))
    })

    # test plot ---------------------------------------------------------------
    output$conditional_plot <- renderUI({
      req(ztest_plot())
      plotOutput(ns('ztest_plot'))
    })

    ztest_plot <- reactive({
      req(ztest$results)
      req(ztest$confidence)

      alt <- ztest$results$values[which(ztest$results$results == 'alternative')]
      z_value <- ztest$results$values[which(ztest$results$results == 'statistic.z')]

      function(){
        plot_z_test(
          confidence = ztest$confidence,
          test_type = alt,
          z_value = z_value |> as.numeric()
        )
      }
    })

    output$ztest_plot <- renderPlot({
      req(ztest$results)
      req(ztest$confidence)
      req(ztest_plot())

      ztest_plot()()
    })

    # staticards --------------------------------------------------------------
    output$conditional_staticard_ztest <- renderUI({
      req(ztest_results_gt())

      z_value <- ztest$results$values[which(ztest$results$results  == 'statistic.z')]
      p_value <- ztest$results$values[which(ztest$results$results == 'p.value')]

      layout_columns(
        col_widths = c(6, 6),
        stati_card(f_num(as.numeric(z_value), dig = 3), 'Z value'),
        stati_card(f_num(as.numeric(p_value), dig = 3), 'p value')
      )
    })

    # help modal --------------------------------------------------------------
    observe({
      fun_help_modal('DescTools', 'ZTest')
    }) |> bindEvent(input$btn_help_ztest)

    # histogram ---------------------------------------------------------------
    output$hist <- renderPlot({
      req(ztest_hist())
      validate(need(input$bins > 0, 'Bins must be 1 or higher'))

      ztest_hist()
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

    ztest_hist <- reactive({ task_hist$result() })

    # insert histogram to output ----------------------------------------------
    insert_output_server(
      'ztest_insert_output_hist',
      reactive(plot_tag(ztest_hist())),
      'Histogram'
    )

  })
}
