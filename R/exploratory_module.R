
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
          card_body(plotOutput(ns('g_dist'))),
          card_footer(
            fluidRow(
              column(8,
                     radioGroupButtons(
                       ns('radio_dist_plot'),
                       'Plot type:',
                       c('Histogram' = 'hist',
                         'Boxplot' = 'boxplot',
                         'Boxplot by Groups' = 'boxplot_group',
                         'Dots' = 'dots',
                         'Barplot' = 'barplot'), size = 'sm', individual = T)),
              column(2, numericInput(ns('var_percentile'), 'Percentile', 50, 0, 100, 5)),
              column(2, conditionalPanel(
                condition = "input.radio_dist_plot == 'hist'", ns = ns,
                numericInput(ns('bins'), 'Bins', 10, 5, step = 10))
              ),
            ),
            div(style = "margin-bottom: -8px !important;"),
          )
        ),
        nav_panel(
          'Scatter',
          full_screen = T,
          card_body(plotOutput(ns('g_scatter'), click = 'plot_brush')),
          card_footer(
            layout_column_wrap(
              checkboxInput(
                ns('scatter_lm'),
                list('Plot Linear Model', bs_icon('info-circle')) |>
                  ttip('Show the line only if LM model was created')),
              btn_task(ns('btn_scatter'), 'Generate Plot', icon('gear'))
            ),
            div(style = "margin-bottom: -18px !important;"),
          )
        ),
        nav_panel(
          'Table',
          full_screen = T,
          card_body(
            radioGroupButtons(ns('table_type'), 'Table type:',
                         c('1 Variable' = '1d',
                           '2 Variables' = '2d'), size = 'sm', individual = T),
            verbatimTextOutput(ns('table'), placeholder = T),
          )
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
            nav_panel('Output', verbatimTextOutput(ns('linear_model'))),
            nav_panel(
              'Residuals',
              plotOutput(ns('g_lm_resid')),
              card_footer(
                layout_column_wrap(
                  radioGroupButtons(ns('radio_lm_resid'), 'Plot type:',
                               c('Histogram' = 'hist', 'Boxplot' = 'boxplot',
                                 'Dots' = 'dots'), size = 'sm', individual = T),
                  btn_task(ns('btn_lm_resid'), 'Plot residuals', icon('chart-simple'))
                ),
                div(style = "margin-bottom: -24px !important;"),
              )
            ),
          )),
      ),
      navset_card_pill(
        nav_panel('Stats', full_screen = T, stats_table_ui(ns('pA_stats')))
      )
    )
  )
)

}

# server ----------------------------------------------------------------------
exploratory_server <- function(id, input_df, df_metadata,
                               color_fill, color_line) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    df <- reactiveValues()
    observe({
      df$df_active <- input_df()
    })

    var_analysis <- reactive({
      df_metadata() |> filter(perc_nas != 1) |>  pull(var)
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
    output$g_dist <- renderPlot({
      req(var())
      req(var2())

      if (input$radio_dist_plot == 'barplot'){
        validate(need(!is.numeric(var()), 'Var can not be numeric'))
        barplot(table(var()), col = color_fill())
      } else {
        validate(need(is.numeric(var()), 'Var must be numeric'))

        if (input$radio_dist_plot == 'boxplot_group'){
          validate(
            need(!is.numeric(var2()) | (is.numeric(var2()) & is.integer(var2())),
                 'Variable 2 can not be float'),
            need(!is.complex(var2()), 'Variable 2 can not be complex')
          )

          g_dist_boxg_col <- colors()[sample.int(
            colors() |> length(), unique(var2()) |> length(), replace = T)]

          boxplot(var() ~ var2(), horizontal = T,
                  col = g_dist_boxg_col, xlab = '', ylab = '')
          abline(v = var_percentile(), col = color_line())
        } else {
          validate(
            need(isTruthy(input$var_percentile)
                 && between(input$var_percentile, 0, 100), 'Percentile must be between 0 and 100')
          )

          if(input$radio_dist_plot == 'hist'){
            validate(need(input$bins > 0, 'Bins must be 1 or higher'))
            hist(var(),
                 col = color_fill(),
                 breaks = input$bins,
                 main = '',
                 xlab = '',
                 ylab = 'Count')
            abline(v = var_percentile(), col = color_line(), lwd = 2)
          } else if (input$radio_dist_plot == 'boxplot'){
            boxplot(var(), horizontal = T, col = color_fill())
            abline(v = var_percentile(), col = color_line(), lwd = 2)
          } else if (input$radio_dist_plot == 'dots'){
            plot(var(), col = color_fill(), ylab = 'Values',
                 pch = if(length(var()) > 1e4) '.' else 20)
            abline(h = var_percentile(), col = color_line(), lwd = 2)
          }
        }
      }
    })
    # render scatter plot -----------------------------------------------------
    output$g_scatter <- renderPlot({
      validate(
        need(is.numeric(var()) && is.numeric(var2()), 'Variables must be numeric')
      )

      if (input$scatter_lm &&
          linear_model$y_name == input$sel_vars &&
          linear_model$x_name == input$sel_vars2) {
        plot(
          var2(),
          var(),
          type = 'p',
          col = color_fill(),
          xlab = input$sel_vars2,
          ylab = input$sel_vars,
          pch = if(length(var()) > 1e4) '.' else 20
        )
        lines(
          linear_model$x,
          linear_model$y,
          col = color_line(),
          lty = 'dotdash',
          lwd = 2
        )
        mtext(paste('Adjusted R Squared:',
                    summary(linear_model$model)$r.squared |> round(4)),
              side = 3)
      } else {
        plot(
          var2(),
          var(),
          type = 'p',
          col = color_fill(),
          xlab = input$sel_vars2,
          ylab = input$sel_vars,
          pch = if(length(var()) > 1e4) '.' else 20
        )
        mtext(paste('Pearson Correlation:', stats_correlation() |> round(4)))
      }
    }) |> bindEvent(input$btn_scatter)

    # tables ------------------------------------------------------------------
    output$table <- renderPrint(
      if(input$table_type == '1d') {
        table(var())
      } else if (input$table_type == '2d'){
        table(var(), var2())
      }
    )

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
      linear_model$x <- NULL
      linear_model$y <- NULL
      linear_model$x_name <- ''
      linear_model$y_name <- ''
      msg('Lm model cleared.')
    }) |> bindEvent(input$btn_scatter_lm_clear)

    # print linear model ------------------------------------------------------
    output$linear_model <- renderPrint({
      list(
        'Formula' = paste(linear_model$y_name, '~', linear_model$x_name),
        'Model' = summary(linear_model$model)
      )
    })

    # plot linear model residuals ---------------------------------------------
    output$g_lm_resid <- renderPlot({

      validate(need(isTruthy(linear_model$model), 'No residuals to plot'))

      if(input$radio_lm_resid == 'hist'){
        hist(linear_model$model$residuals,
             col = color_fill(),
             main = '',
             xlab = '',
             ylab = 'Count')
      } else if (input$radio_lm_resid == 'boxplot'){
        boxplot(linear_model$model$residuals,
                horizontal = T, col = color_fill())
      } else if (input$radio_lm_resid == 'dots'){
        plot(linear_model$model$residuals, col = color_fill(),
             ylab = 'Residuals', pch = 19)
        abline(h = 0, col = color_line(), lty = 'dotdash', lwd = 2)
      }
    }) |> bindEvent(input$btn_lm_resid)

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
    stats_table_server('pA_stats', var, var2,
                       reactive(input$var_percentile),
                       var_percentile,
                       stats_sd, stats_correlation)

  })
}
