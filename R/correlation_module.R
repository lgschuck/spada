
# ui --------------------------------------------------------------------------
correlation_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Correlation', class = 'mini-header'),
    layout_sidebar(bg = '#02517d',
      sidebar = sidebar(uiOutput(ns('parameters')), bg = '#e3e3e4'),
      navset_card_pill(
        nav_panel('Test', accordion(open = T,
          accordion_panel('Parameters', fluidRow(
            column(3, radioButtons(
              ns('radio_method'),
              'Method',
              c(
                'Pearson' = 'pearson',
                'Kendall' = 'kendall',
                'Spearman' = 'spearman'
              )
            )),
            column(3, radioButtons(
              ns('radio_alternative'), 'Alternative', c(
                c(
                  'Two.sided' = 'two.sided',
                  'Less' = 'less',
                  'Greater' = 'greater'
                )
              )
            )),
            column(
              2,
              numericInput(ns('confidence'), 'Confidence Interval - %', value = 95, 0, 100, 5),
              btn_task(ns('btn_run_test'), 'Run Test')
            )
          )),
          accordion_panel('Test Results', verbatimTextOutput(ns(
            'cor_test_results'
          )))
        )),
        nav_panel('Plot', plotOutput(ns('scatter_plot'))),
      )
    )
  )
}

# server ----------------------------------------------------------------------
correlation_server <- function(id, df, metadata, color_fill) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    cor_test <- reactiveValues(results = NULL)

    df_active <- reactive(df()[, lapply(df(), is.numeric) == T, with = F])

    var_analysis <- reactive({
      df_names <- df_active() |> names()

      var_analysis <- metadata() |> filter(perc_nas != 1) |> pull(var)

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

      if(!isTruthy(input$confidence) || !between(input$confidence, 0, 100)){
        msg('Inform a value between 0 and 100 %', 2)
      } else {
        cor_test$results <- cor.test(df_active()[[input$sel_var1]],
                                     df_active()[[input$sel_var2]],
                                     alternative = input$radio_alternative,
                                     method = input$radio_method,
                                     conf.level = input$confidence/100,
                                     exact = F)

        msg('Test completed')
      }
    }) |> bindEvent(input$btn_run_test)

    output$cor_test_results <- renderPrint(cor_test$results) |>
      bindEvent(input$btn_run_test)

    output$scatter_plot <- renderPlot(
      plot(df_active()[[input$sel_var1]],
           df_active()[[input$sel_var2]],
           xlab = input$sel_var1,
           ylab = input$sel_var2,
           col = color_fill(), pch = 19, cex = 0.8
      )

    )|> bindCache(df_active(), input$sel_var1, input$sel_var2, color_fill())
  })
}
