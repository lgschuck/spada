
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
          card_body(plotOutput(ns('g_scatter'))),
          card_footer(
            layout_column_wrap(
              checkboxInput(
                ns('scatter_lm'),
                list('Plot Linear Model', bs_icon('info-circle')) |>
                  ttip('Show the line only if LM model was created')),
              btn_task(ns('btn_scatter'), 'Generate Plot', icon('chart-simple'))
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

        ggplot(data = data.frame(x = var()), aes(x = factor(x))) +
          geom_bar(fill = color_fill()) +
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

          ggplot(data = data.frame(x = var2(), y = var()),
                 aes(x = x, y = y, fill = x)) +
            stat_boxplot(geom = 'errorbar', width = 0.3) +
            geom_boxplot(orientation = 'x') +
            geom_hline(yintercept = var_percentile(),  color = color_line()) +
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

            ggplot(data = data.frame(x = var()), aes(x = x)) +
              geom_histogram(
                bins = input$bins,
                fill = color_fill(),
                color = '#000000'
              ) +
              geom_vline(xintercept = var_percentile(), color = color_line()) +
              labs(x = '', y = 'Count', title = '') +
              theme_classic() +
              theme(axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    axis.title.y = element_text(size = 16)
                    )

          } else if (input$radio_dist_plot == 'boxplot'){
            ggplot(data = data.frame(x = var()), aes(x = x)) +
              stat_boxplot(geom = 'errorbar', width = 0.3) +
              geom_boxplot(fill = color_fill()) +
              ylim(-1.2, 1.2) +
              geom_vline(xintercept = var_percentile(), color = color_line()) +
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
              geom_point(shape = point_shape, color = color_fill()) +
              geom_hline(yintercept = var_percentile(), color = color_line()) +
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
    }, res = 96)
    # render scatter plot -----------------------------------------------------
    output$g_scatter <- renderPlot({
      validate(
        need(is.numeric(var()) && is.numeric(var2()), 'Variables must be numeric')
      )

      point_shape <- if(length(var()) > 1e4) "." else 20

      if (input$scatter_lm &&
          linear_model$y_name == input$sel_vars &&
          linear_model$x_name == input$sel_vars2) {

        ggplot(data.frame(x = var2(), y = var()), aes(x = x, y = y)) +
          geom_point(color = color_fill(), shape = point_shape) +
          geom_line(
            data.frame(x = linear_model$x, y = linear_model$y),
            aes(x = x, y = y),
            color = color_line(),
            linewidth = 1
          ) +
          labs(
            title = paste(
              'Adjusted R Squared:',
              summary(linear_model$model)$r.squared |> round(4)
            ),
            x = input$sel_vars2,
            y = input$sel_vars
          )+
          theme_classic() +
          theme(axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16)
                )

      } else {
        ggplot(data.frame(x = var2(), y = var()), aes(x = x, y = y)) +
          geom_point(color = color_fill(), shape = point_shape) +
          labs(title = paste('Pearson Correlation:', stats_correlation() |> round(4)),
               x = input$sel_vars2, y = input$sel_vars) +
          theme_classic() +
          theme(axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16)
                )
      }
    }, res = 96) |> bindEvent(input$btn_scatter)

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
        ggplot(data = data.frame(x = linear_model$model$residuals), aes(x = x)) +
          geom_histogram(bins = 10, fill = color_fill(), color = '#000000') +
          labs(x = '', y = 'Count', title = '') +
          theme_classic() +
          theme(axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.y = element_text(size = 16)
          )

      } else if (input$radio_lm_resid == 'boxplot'){
        ggplot(data = data.frame(x = linear_model$model$residuals), aes(x = x)) +
          stat_boxplot(geom = 'errorbar', width = 0.3) +
          geom_boxplot(fill = color_fill()) +
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
          geom_point(shape = point_shape, color = color_fill()) +
          geom_hline(yintercept = 0, color = color_line(), linetype = 2) +
          labs(x = 'Index', y = 'Values') +
          theme_classic() +
          theme(axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16)
          )
      }
    }, res = 96) |> bindEvent(input$btn_lm_resid)

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
