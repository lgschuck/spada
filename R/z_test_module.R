
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
                width = 380,
                fluidRow(
                  h5('Sampe Values'),
                  column(6, p(textOutput(ns('sample_mean')))),
                  column(6, p(textOutput(ns('sample_sd'))))
                ),
                h5('Parameters', style = 'margin-bottom: -18px;'),
                layout_columns(
                  numericInput(
                    ns('mu'),
                    list('Mean', bs_icon('info-circle') |>
                           ttip('Hypothesized Mean')), 0),
                  numericInput(
                    ns('sd'),
                    list('Std Deviation', bs_icon('info-circle') |>
                           ttip('Standard Deviation of Population')),
                    value = 1, min = 0)
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
                  btn_task(ns('btn_help_ztest'), 'Help', icon('question'))
                )
              ),
              card_body(
                layout_columns(
                  col_widths = c(2, 7, 3),
                  fluidRow(
                    div(
                      style = 'margin-bottom: 10px;',
                      uiOutput(ns('conditional_staticard_ztest')),
                    )
                  ),
                  uiOutput(ns('conditional_plot')),
                  div(
                    gt_output(ns('ztest_gt')),
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

      var_analysis <- session$userData$dt$act_meta() |> filter(perc_nas != 1) |> pull(var)

      df_names[df_names %in% var_analysis]
    })

    var <- reactive({
      req(input$sel_var)
      df_active()[[input$sel_var]]
    })

    # calculate values --------------------------------------------------------
    sample_mean <- reactive(var() |> mean(na.rm = T))

    output$sample_mean <- renderText({
      paste('Mean:', sample_mean() |> f_num(dig = 2))
    })

    sample_sd <- reactive(var() |> sd(na.rm = T))

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
      } else if (sum(!is.na(var())) < 2) {
        msg_error('Inform at least 2 valid values2')
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
    mod_insert_output <- insert_output_server(
      'ztest_insert_output',
      reactive(
        gen_table2(
          plotTag(ztest_plot()(), '', width = 1000, height = 500),
          ztest_results_gt()
        )
      )
    )

    output$conditional_add_output <- renderUI({
      req(ztest_results_gt())
      insert_output_ui(ns('ztest_insert_output'))
    })

    # get return from insert output module ------------------------------------
    observe({
      req(mod_insert_output$output_element())
      req(mod_insert_output$output_element()$id)

      session$userData$out$elements[[mod_insert_output$output_element()$id]] <- mod_insert_output$output_element()

    }) |> bindEvent(mod_insert_output$output_element())

    # test plot ---------------------------------------------------------------
    output$conditional_plot <- renderUI({
      req(ztest_plot())
      plotOutput(ns('ztest_plot'))
    })

    ztest_plot <- reactive({
      req(ztest$results)
      req(ztest$confidence)

      function(){
        plot_z_test(
          confidence = ztest$confidence,
          test_type = ztest$results |>
            filter(results == 'alternative') |>
            pull(values),
          z_value = ztest$results |>
            filter(results == 'statistic.z') |>
            pull(values) |>
            as.numeric()
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
      tagList(
        stati_card(ztest$results |>
                   filter(results %in% c('statistic.z')) |>
                   pull(values) |>
                   as.numeric() |>
                   f_num(dig = 3),
                   'Z value'),
        stati_card(ztest$results |>
                   filter(results %in% c('p.value')) |>
                   pull(values) |>
                   as.numeric() |>
                   f_num(dig = 3),
                   'p value')
      )
    })

    # help modal --------------------------------------------------------------
    observe({
      showModal(modalDialog(
        HTML(get_help_file('DescTools', 'ZTest')),
        easyClose = TRUE, size = 'xl'
      ))
    }) |> bindEvent(input$btn_help_ztest)

    # histogram ---------------------------------------------------------------
    output$hist <- renderPlot({
      req(ztest_hist())
      validate(need(input$bins > 0, 'Bins must be 1 or higher'))

      ztest_hist()
    }, res = 96)

    ztest_hist <- reactive({
      req(df_active())
      req(input$sel_var)

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
                 mean_value = mean(var(), na.rm = TRUE),
                 sd_value = sd(var(), na.rm = TRUE)
      )
    }) |> bindEvent(input$btn_hist)

    # insert histogram to output ----------------------------------------------
    mod_insert_output_hist <- insert_output_server(
      'ztest_insert_output_hist',
      reactive(
        plotTag(ztest_hist(), '', width = 1000, height = 500)
      )
    )

    # get return from insert output module ------------------------------------
    observe({
      req(mod_insert_output_hist$output_element())
      req(mod_insert_output$output_element()$id)

      session$userData$out$elements[[mod_insert_output_hist$output_element()$id]] <- mod_insert_output_hist$output_element()

    }) |> bindEvent(mod_insert_output_hist$output_element())

  })
}
