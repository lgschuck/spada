
# ui --------------------------------------------------------------------------
z_test_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Z Test', class = 'mini-header'),
    layout_sidebar(
      bg = '#02517d',
      sidebar = sidebar(uiOutput(ns('parameters')), bg = '#e3e3e4'),
      navset_card_pill(
        nav_panel(
          'Test',
          card(
            layout_sidebar(
              sidebar = sidebar(
                width = 400,
                h5('Parameters', style = 'margin-bottom: -18px;'),
                numericInput(ns('mu'), 'Hypothesized Mean', 0),
                numericInput(ns('sd'), 'Std Deviation of Population', 1),
                radioButtons(ns('radio_alternative'), 'Alternative',
                             c('Two.sided' = 'two.sided',
                               'Less' = 'less',
                               'Greater' = 'greater')),
                numericInput(ns('confidence'), 'Confidence Interval - %',
                             value = 95, 0, 100, 5, width = '200px'),
                layout_columns(
                  col_widths = c(6, 6),
                  btn_task(ns('btn_run_test'), 'Run Test', icon('gear')),
                  btn_task(ns('btn_help_ztest'), 'Help', icon('question'))
                )
              ),
              card_body(
                layout_column_wrap(
                  uiOutput(ns('staticard_mean')),
                  uiOutput(ns('staticard_sd'))
                ),
                layout_columns(
                  col_widths = c(3, 7, 2),
                  uiOutput(ns('conditional_staticard_ztest')),
                  gt_output(ns('ztest_gt')),
                  uiOutput(ns('conditional_save_gt'))
                )
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
              div(style = "margin-bottom: -8px !important;"),
              layout_columns(
                col_widths = c(2, 3),
                numericInput(ns('bins'), 'Number of Bins', 30, 5, step = 5),
                btn_task(ns('btn_hist'), 'Generate Histogram', icon('gear'),
                         style = 'margin-top: 20px')
              ),
              div(style = "margin-bottom: -24px !important;"),
            )
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
z_test_server <- function(id, df, df_metadata, color_fill, color_line) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    ztest <- reactiveValues(results = NULL)

    df_active <- reactive(df()[, lapply(df(), is.numeric) == T, with = F])

    var_analysis <- reactive({
      df_names <- df_active() |> names()

      var_analysis <- df_metadata() |> filter(perc_nas != 1) |> pull(var)

      df_names[df_names %in% var_analysis]
      })

    var <- reactive({
      req(input$sel_var)
      df_active()[[input$sel_var]]
    })

    sample_mean <- reactive(var() |> mean(na.rm = T))

    sample_sd <- reactive(var() |> sd(na.rm = T))

    output$parameters <- renderUI({
      tagList(
        selectInput(ns('sel_var'), 'Variable', var_analysis()),
        p('* Showing only numeric variables')
      )
    })

    observe({
      req(input$sel_var)
      req(input$radio_alternative)

      if(input$sd == 0){
        msg_error('Standard Deviation can not be 0', 2)
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

        msg('Test completed', DURATION = 1.5)
      }
    }) |> bindEvent(input$btn_run_test)

    ztest_results_gt <- reactive({
      req(ztest$results)
      ztest$results |>
        gt() |>
        cols_move(columns = 'values', after = 'results') |>
        gt::cols_label('values' = 'Values', 'results' = 'Results')
    })

    output$ztest_gt <- render_gt({
      req(ztest_results_gt())
      ztest_results_gt()
    })

    save_gt_server('ztest_save_gt', ztest_results_gt)

    output$conditional_save_gt <- renderUI({
      req(ztest_results_gt())
      save_gt_ui(ns('ztest_save_gt'))
    })

    output$staticard_mean <- renderUI({
      req(sample_mean())
      statiCard(sample_mean() |> f_num(dig = 3),
                subtitle = paste(input$sel_var, '- Mean'),
                left = T,
                animate = T,
                duration = 30)
    })

    output$staticard_sd <- renderUI({
      req(sample_sd())
      statiCard(sample_sd() |> f_num(dig = 3),
                subtitle = paste(input$sel_var, '- Std Deviation'),
                left = T,
                animate = T,
                duration = 30)
    })

    output$conditional_staticard_ztest <- renderUI({
      req(ztest_results_gt())
      tagList(
        statiCard(ztest$results |>
                  filter(results %in% c('statistic.z')) |>
                  pull(values) |>
                  as.numeric() |>
                  f_num(dig = 3),
                  subtitle = 'Z value',
                  left = T,
                  animate = T,
                  duration = 30),
        statiCard(ztest$results |>
                  filter(results %in% c('p.value')) |>
                  pull(values) |>
                  as.numeric() |>
                  f_num(dig = 3),
                  subtitle = 'p value',
                  left = T,
                  animate = T,
                  duration = 30)
      )
    })

    observe({
      showModal(modalDialog(
        HTML(get_help_file('DescTools', 'ZTest')),
        easyClose = TRUE, size = 'xl'
      ))
    }) |> bindEvent(input$btn_help_ztest)

    output$ztest_results <- renderPrint(ztest$results) |>
      bindEvent(input$btn_run_test)

    # histogram ---------------------------------------------------------------
    output$hist <- renderPlot({
      req(df_active())
      req(input$sel_var)

      validate(need(input$bins > 0, 'Bins must be 1 or higher'))

      hist(var(),
           breaks = input$bins,
           probability = TRUE,
           col = color_fill(),
           xlab = 'Values',
           ylab = 'Density',
           main = paste('Histogram of', input$sel_var)
      )
      curve(dnorm(x, mean = mean(var(), na.rm = T),
                  sd = sd(var(), na.rm = T)),
            col = color_line(),
            lwd = 2,
            add = TRUE)
    }) |> bindCache(var(), color_fill(), input$bins) |>
      bindEvent(input$btn_hist)

  })
}
