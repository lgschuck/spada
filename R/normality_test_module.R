
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
                col_widths = c(1, 3, 3),
                numericInput(ns('bins'), 'Number of Bins', 30, 5, step = 5),
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

	  df <- reactive(session$userData$df$act)

	  # outupt objects ----------------------------------------------------------
	  output_list <- reactiveValues(elements = NULL)

	  observe({
	    output_list$elements <- session$userData$out$elements
	  })

    df_active <- reactive({
      req(df())
      df()[, lapply(df(), is.numeric) == T, with = F]
    })

    var_analysis <- reactive({
      req(df_active())

      df_names <- df_active() |> names()

      var_analysis <- session$userData$df$act_meta() |> filter(perc_nas != 1) |> pull(var)

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

      ggplot(data.frame(x = var()), aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)),
                       bins = input$bins,
                       fill = session$userData$fill_color,
                       color = "black") +
        stat_function(fun = dnorm,
                      args = list(mean = mean(var(), na.rm = TRUE),
                                  sd = sd(var(), na.rm = TRUE)),
                      color = session$userData$line_color,
                      linewidth = 1) +
        labs(x = "Values",
             y = "Density") +
        theme_classic() +
        theme(axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16)
        )
    }) |> bindEvent(input$btn_hist)

    # insert histogram to output ----------------------------------------------
    mod_insert_output_hist <- insert_output_server(
      'norm_insert_output_hist',
      reactive(
        plotTag(norm_hist(), '', width = 1000, height = 500)
      )
    )

    # get return from insert output module ------------------------------------
    observe({
      req(mod_insert_output_hist$output_element())

      output_list$elements[[gen_element_id()]] <- mod_insert_output_hist$output_element()

    }) |> bindEvent(mod_insert_output_hist$output_element())

    # qq plot -----------------------------------------------------------------
    output$qq_plot <- renderPlot({
      norm_qq_plot()
    }, res = 96)

    norm_qq_plot <- reactive({
      req(input$sel_var)
      req(var())

      ggplot(data.frame(x = var()), aes(sample = x)) +
        stat_qq(color = session$userData$fill_color) +
        stat_qq_line(color = session$userData$line_color) +
        labs(title = paste('Normal QQ Plot:', input$sel_var),
             x = 'Theoretical Quantiles',
             y = 'Sample Quantiles') +
        theme_classic() +
        theme(axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16)
        )
    }) |> bindEvent(input$btn_qq)

    # insert to output --------------------------------------------------------
    mod_insert_output_qq <- insert_output_server(
      'norm_insert_output_qq',
      reactive(plotTag(norm_qq_plot(), '', width = 1000, height = 500))
    )

    output$conditional_add_output_qq <- renderUI({
      req(norm_qq_plot())
      insert_output_ui(ns('norm_insert_output_qq'))
    })

    # get return from insert output module ------------------------------------
    observe({
      req(mod_insert_output_qq$output_element())

      output_list$elements[[gen_element_id()]] <- mod_insert_output_qq$output_element()

    }) |> bindEvent(mod_insert_output_qq$output_element())

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
    mod_insert_output_ks <- insert_output_server(
      'norm_insert_output_ks',
      reactive(ks_results_gt())
    )

    output$conditional_add_output_ks <- renderUI({
      req(ks_results_gt())
      insert_output_ui(ns('norm_insert_output_ks'))
    })

    # get return from insert output module ------------------------------------
    observe({
      req(mod_insert_output_ks$output_element())

      output_list$elements[[gen_element_id()]] <- mod_insert_output_ks$output_element()

    }) |> bindEvent(mod_insert_output_ks$output_element())

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
    mod_insert_output_sw <- insert_output_server(
      'norm_insert_output_sw',
      reactive(sw_results_gt())
    )

    output$conditional_add_output_sw <- renderUI({
      req(sw_results_gt())
      insert_output_ui(ns('norm_insert_output_sw'))
    })

    # get return from insert output module ------------------------------------
    observe({
      req(mod_insert_output_sw$output_element())

      output_list$elements[[gen_element_id()]] <- mod_insert_output_sw$output_element()

    }) |> bindEvent(mod_insert_output_sw$output_element())

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
    mod_insert_output_sf <- insert_output_server(
      'norm_insert_output_sf',
      reactive(sf_results_gt())
    )

    output$conditional_add_output_sf <- renderUI({
      req(sf_results_gt())
      insert_output_ui(ns('norm_insert_output_sf'))
    })

    # get return from insert output module ------------------------------------
    observe({
      req(mod_insert_output_sf$output_element())

      output_list$elements[[gen_element_id()]] <- mod_insert_output_sf$output_element()

    }) |> bindEvent(mod_insert_output_sf$output_element())

    # help file of ShapiroFranciaTest
    observe({
      showModal(modalDialog(
        HTML(get_help_file('DescTools', 'ShapiroFranciaTest')),
        easyClose = TRUE, size = 'xl'
      ))
    }) |> bindEvent(input$btn_help_sf)

    # update output -----------------------------------------------------------
    observe({
      session$userData$out$elements <- output_list$elements
    })

  })
}
