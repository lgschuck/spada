
# ui --------------------------------------------------------------------------
correlation_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Correlation Test', class = 'mini-header'),
    layout_sidebar(
      class = 'card-sidebar',
      sidebar = sidebar(uiOutput(ns('parameters'))),
      navset_card_pill(
        nav_panel(
          'Test',
          card(
            layout_sidebar(
              sidebar = sidebar(
                width = 400,
                h5('Parameters', style = 'margin-bottom: -18px;'),
                radioButtons(ns('radio_method'), 'Method',
                             c('Pearson' = 'pearson',
                               'Kendall' = 'kendall',
                               'Spearman' = 'spearman')),
                radioButtons(ns('radio_alternative'), 'Alternative',
                             c('Two sided' = 'two.sided',
                               'Less' = 'less',
                               'Greater' = 'greater'), inline = T),
                numericInput(ns('confidence'), 'Confidence Interval - %',
                             value = 95, 0, 100, 5, width = '200px'),
                layout_columns(
                  col_widths = c(6, 6),
                  btn_task(ns('btn_run_test'), 'Run Test', icon('gear')),
                  btn_task(ns('btn_help_cor'), 'Help', icon('question'))
                )
              ),
              layout_columns(
                col_widths = c(3, 7, 2),
                uiOutput(ns('conditional_staticard_cor')),
                gt_output(ns('cor_gt')),
                uiOutput(ns('conditional_save_gt'))
              )
            )
          )
        ),
        nav_panel(
          'Scatter',
          card(
            full_screen = T,
            card_body(plotOutput(ns('scatter_plot'))),
            card_footer(
              btn_task(ns('btn_scatter'), 'Generate Plot', icon('gear'))
            )
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
correlation_server <- function(id, df, df_metadata, color_fill) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    cor_test <- reactiveValues(results = NULL)

    df_active <- reactive(df()[, lapply(df(), is.numeric) == T, with = F])

    var_analysis <- reactive({
      df_names <- df_active() |> names()

      var_analysis <- df_metadata() |> filter(perc_nas != 1) |> pull(var)

      df_names[df_names %in% var_analysis]
      })

    output$parameters <- renderUI({
      tagList(
        selectInput(ns('sel_var1'), 'Variable 1', var_analysis()),
        selectInput(ns('sel_var2'), 'Variable 2', var_analysis(),
                    var_analysis()[2]),
        p('* Showing only numeric variables')
      )
    })

    observe({
      req(input$sel_var1)
      req(input$sel_var2)
      req(input$radio_alternative)
      req(input$radio_method)

      if(!isTruthy(input$confidence) ||
         !between(input$confidence, 0, 100)) {
        msg_error('Confidence interval must be between 0 and 100%', 2)
      } else if (sum(!is.na(df_active()[[input$sel_var1]])) < 3 ||
                 sum(!is.na(df_active()[[input$sel_var2]])) < 3) {
        msg_error('Inform at least 3 valid values for each variable')
      } else if (sd(df_active()[[input$sel_var1]], na.rm = T) == 0 ||
                 sd(df_active()[[input$sel_var2]], na.rm = T) == 0) {
        msg_error('The Standard deviation of any of the variables can not be zero', 3)
      } else {
        df <- cor.test(df_active()[[input$sel_var1]],
                       df_active()[[input$sel_var2]],
                       alternative = input$radio_alternative,
                       method = input$radio_method,
                       conf.level = input$confidence/100,
                       exact = F)

        df <- df |>
          unlist() |> as.data.frame()

        df$results <- rownames(df)
        names(df) <- c('values', 'results')

        df[df$results == 'data.name', ]$values <-
          paste(input$sel_var1, '/', input$sel_var2)

        cor_test$results <- df

        msg('Test completed', DURATION = 1.5)
      }
    }) |> bindEvent(input$btn_run_test)

    cor_results_gt <- reactive({
      req(cor_test$results)
      cor_test$results |>
        gt() |>
        cols_move(columns = 'values', after = 'results') |>
        gt::cols_label('values' = 'Values', 'results' = 'Results')
    })

    output$cor_gt <- render_gt({
      req(cor_results_gt())
      cor_results_gt()
    })

    save_gt_server('cor_save_gt', cor_results_gt)

    output$conditional_save_gt <- renderUI({
      req(cor_results_gt())
      save_gt_ui(ns('cor_save_gt'))
    })

    output$conditional_staticard_cor <- renderUI({
      req(cor_results_gt())
      tagList(
        stati_card(cor_test$results |>
                  filter(results %in% c('estimate.cor', 'estimate.tau',
                  'estimate.rho')) |>
                  pull(values) |>
                  as.numeric() |>
                  f_num(dig = 3),
                  'Correlation'),
        stati_card(cor_test$results |>
                  filter(results %in% c('p.value')) |>
                  pull(values) |>
                  as.numeric() |>
                  f_num(dig = 3),
                  'p value')
      )
    })

    observe({
      showModal(modalDialog(
        HTML(get_help_file('stats', 'cor.test')),
        easyClose = TRUE, size = 'xl'
      ))
    }) |> bindEvent(input$btn_help_cor)

    output$cor_test_results <- renderPrint(cor_test$results) |>
      bindEvent(input$btn_run_test)

    output$scatter_plot <- renderPlot({
      req(input$sel_var1)
      req(input$sel_var2)

      plot(df_active()[[input$sel_var1]],
           df_active()[[input$sel_var2]],
           xlab = input$sel_var1,
           ylab = input$sel_var2,
           col = color_fill(), pch = 19, cex = 0.8
      )

    })|> bindCache(df_active(), input$sel_var1, input$sel_var2, color_fill()) |>
      bindEvent(input$btn_scatter)
  })
}
