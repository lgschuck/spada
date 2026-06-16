
# ui --------------------------------------------------------------------------
one_t_test_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('One-sample t Test', class = 'mini-header'),
    layout_sidebar(
      class = 'card-sidebar',
      sidebar = sidebar(uiOutput(ns('parameters'))),
      navset_card_pill(
        nav_panel(
          'Test',
          card(
            layout_sidebar(
              sidebar = sidebar(
                width = 380,
                fluidRow(
                  h5('Sampe Values'),
                  column(6, p(textOutput(ns('sample_mean'))))
                ),
                h5('Parameters', style = 'margin-bottom: -18px;'),
                layout_columns(
                  numericInput(
                    ns('mu'),
                    list('Mean', bs_icon('info-circle') |>
                           ttip('True Value of Mean')), 0)
                ),
                radioButtons(ns('radio_alternative'), 'Alternative',
                             c('Two sided' = 'two.sided',
                               'Less' = 'less',
                               'Greater' = 'greater'), inline = T),
                numericInput(ns('confidence'), 'Confidence Interval - %',
                             value = 95, 0, 100, 5, width = '200px'),
                layout_columns(
                  col_widths = c(6, 6),
                  btn_task(ns('btn_run_test'), 'Run Test', icon('gear')),
                  btn_task(ns('btn_help_ttest'), 'Help', icon('question'))
                )
              ),
              card_body(
                layout_columns(
                  col_widths = c(2, 7, 3),
                  fluidRow(
                    div(
                      style = 'margin-bottom: 10px;',
                      uiOutput(ns('conditional_staticard_ttest')),
                    )
                  ),
                  uiOutput(ns('conditional_plot')),
                  div(
                    gt_output(ns('ttest_gt')),
                    br(), br(),
                    uiOutput(ns('conditional_save_gt'))
                  )
                )
              ),
              card_footer(
                uiOutput(ns('conditional_add_output'))
              )
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
                div(insert_output_ui(ns('ttest_insert_output_hist')),
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
one_t_test_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  df <- reactive(get_act_dt(session))

    ttest <- reactiveValues(results = NULL)

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

    # input vars --------------------------------------------------------------
    output$parameters <- renderUI({
      tagList(
        selectInput(ns('sel_var'), 'Variable', var_analysis()),
        p('* Showing only numeric variables')
      )
    })

    # run t test --------------------------------------------------------------
    observe({
      req(input$sel_var)
      req(input$radio_alternative)

      if(!isTruthy(input$mu)){
        msg_error('Inform a value for the Mean')
      } else if(!isTruthy(input$confidence) ||
         !between(input$confidence, 0, 100)) {
        msg_error('Confidence interval must be between 0 and 100%', 2)
      } else if (fnobs(var()) < 2) {
        msg_error('Inform at least 2 valid values')
      } else {
        df <- t.test(var(),
                    alternative = input$radio_alternative,
                    mu = input$mu,
                    conf.level = input$confidence/100)

        df <- df |>
          unlist() |> as.data.frame()

        df$results <- rownames(df)
        names(df) <- c('values', 'results')

        df[df$results == 'data.name', ]$values <- input$sel_var

        ttest$results <- df

        ttest$confidence <- input$confidence/100
      }
    }) |> bindEvent(input$btn_run_test)

    # results gt table --------------------------------------------------------
    ttest_results_gt <- reactive({
      req(ttest$results)
      ttest$results |>
        gt() |>
        cols_move(columns = 'values', after = 'results') |>
        cols_label('values' = 'Values', 'results' = 'Results') |>
        tab_header('One-sample t Test')
    })

    output$ttest_gt <- render_gt({
      req(ttest_results_gt())
      ttest_results_gt()
    })

    # save gt module ----------------------------------------------------------
    save_gt_server('ttest_save_gt', ttest_results_gt)

    output$conditional_save_gt <- renderUI({
      req(ttest_results_gt())
      save_gt_ui(ns('ttest_save_gt'))
    })

    # insert to output --------------------------------------------------------
    insert_output_server(
      'ttest_insert_output',
      reactive(
        gen_table2(
          plot_tag(ttest_plot()(), w = 600, h = 300),
          ttest_results_gt(),
          '75%',
          '25%'
        )
      ),
      'One-sample t Test'
    )

    output$conditional_add_output <- renderUI({
      req(ttest_results_gt())
      insert_output_ui(ns('ttest_insert_output'))
    })

    # test plot ---------------------------------------------------------------
    output$conditional_plot <- renderUI({
      req(ttest_plot())
      plotOutput(ns('ttest_plot'))
    })

    ttest_plot <- reactive({
      req(ttest$results)
      req(ttest$confidence)

      alt <- ttest$results$values[which(ttest$results$results == 'alternative')]

      t_value <- ttest$results$values[which(ttest$results$results == 'statistic.t')]

      df <- ttest$results$values[which(ttest$results$results == 'parameter.df')]

      function(){
        plot_t_test(
          confidence = ttest$confidence,
          df = df |> as.numeric(),
          test_type = alt,
          t_value = t_value |> as.numeric()
        )
      }
    })

    output$ttest_plot <- renderPlot({
      req(ttest$results)
      req(ttest$confidence)
      req(ttest_plot())

      ttest_plot()()
    })

    # staticards --------------------------------------------------------------
    output$conditional_staticard_ttest <- renderUI({
      req(ttest_results_gt())

      t_value <- ttest$results$values[which(ttest$results$results == 'statistic.t')]
      p_value <- ttest$results$values[which(ttest$results$results == 'p.value')]

      tagList(
        stati_card(f_num(as.numeric(t_value), dig = 3), 't value'),
        stati_card(f_num(as.numeric(p_value), dig = 3), 'p value')
      )
    })

    # help modal --------------------------------------------------------------
    observe({
      fun_help_modal('stats', 't.test')
    }) |> bindEvent(input$btn_help_ttest)

    # histogram ---------------------------------------------------------------
    output$hist <- renderPlot({
      req(ttest_hist())
      validate(need(input$bins > 0, 'Bins must be 1 or higher'))

      ttest_hist()
    }, res = 96)

    task_hist <- ExtendedTask$new(function(spada_plot_fun,
                                           df,
                                           title,
                                           bins,
                                           plot_fill_color,
                                           plot_line_color,
                                           sample_limit,
                                           mean_value,
                                           sd_value) {
      mirai({
        spada_plot_fun(
          type = 'hist_density',
          df = df,
          xvar = 'x',
          xlab = 'Values',
          ylab = 'Density',
          title = title,
          bins = bins,
          fill_color = plot_fill_color,
          line_color = plot_line_color,
          point_shape = if (sample_limit > 1e4 && nrow(df) > 1e4) '.' else 20,
          sample_limit = sample_limit,
          mean_value = mean_value,
          sd_value = sd_value
        )
      },
      spada_plot_fun = spada_plot_fun,
      df = df,
      title = title,
      bins = bins,
      plot_fill_color = plot_fill_color,
      plot_line_color = plot_line_color,
      sample_limit = sample_limit,
      mean_value = mean_value,
      sd_value = sd_value)
    }) |> bind_task_button('btn_hist')

    observe({
      req(input$sel_var, var())
      task_hist$invoke(
        spada_plot_fun = spada_plot,
        df = data.frame(x = var()),
        bins = input$bins,
        title = paste('Histogram -', input$sel_var),
        plot_fill_color = session$userData$conf$plot_fill_color,
        plot_line_color = session$userData$conf$plot_line_color,
        sample_limit = session$userData$conf$plot_limit,
        mean_value = fmean(var(), na.rm = TRUE),
        sd_value = fsd(var(), na.rm = TRUE)
      )
    }) |> bindEvent(input$btn_hist)

    ttest_hist <- reactive({ task_hist$result() })

    # insert histogram to output ----------------------------------------------
    insert_output_server(
      'ttest_insert_output_hist',
      reactive(plot_tag(ttest_hist())),
      'Histogram'
    )

  })
}
