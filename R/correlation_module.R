
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
                col_widths = c(3, 6, 3),
                uiOutput(ns('conditional_staticard_cor')),
                gt_output(ns('cor_gt')),
                div(
                  uiOutput(ns('cond_add_output')),
                  br(), br(),
                  uiOutput(ns('conditional_save_gt'))
                ),
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
              layout_columns(
                col_widths = c(3, 3),
                btn_task(ns('btn_scatter'), 'Show Scatter Plot', icon('chart-simple')),
                insert_output_ui(ns('insert_scatter'))
              )
            )
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
correlation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  df <- reactive(get_act_dt(session))

    # outupt objects ----------------------------------------------------------
	  output_list <- reactiveValues(elements = NULL)

	  observe({
	    output_list$elements <- session$userData$out$elements
	  })

    cor_test <- reactiveValues(results = NULL)

    df_active <- reactive(df()[, lapply(df(), is.numeric) == T, with = F])

    var_analysis <- reactive({
      df_names <- df_active() |> names()

      var_analysis <- session$userData$dt$act_meta() |> filter(perc_nas != 1) |> pull(var)

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

    # run test events ---------------------------------------------------------
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
      }
    }) |> bindEvent(input$btn_run_test)

    # results -----------------------------------------------------------------
    cor_results_gt <- reactive({
      req(cor_test$results)
      cor_test$results |>
        gt() |>
        cols_move(columns = 'values', after = 'results') |>
        cols_label('values' = 'Values', 'results' = 'Results') |>
        tab_header('Correlation Test')
    })

    output$cor_gt <- render_gt({
      req(cor_results_gt())
      cor_results_gt()
    })

    # save gt -----------------------------------------------------------------
    save_gt_server('cor_save_gt', cor_results_gt)

    output$conditional_save_gt <- renderUI({
      req(cor_results_gt())
      save_gt_ui(ns('cor_save_gt'))
    })

    # insert to output --------------------------------------------------------
    mod_output_gt <- insert_output_server('insert_gt', cor_results_gt)

    output$cond_add_output <- renderUI({
      req(cor_results_gt())
      insert_output_ui(ns('insert_gt'))
    })

    # get return from insert output module ------------------------------------
    observe({
      req(mod_output_gt$output_element())

      output_list$elements[[gen_element_id()]] <- mod_output_gt$output_element()

    }) |> bindEvent(mod_output_gt$output_element())

    # staticards --------------------------------------------------------------
    cor_staticards <- reactive({
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

    output$conditional_staticard_cor <- renderUI({
      req(cor_staticards())
      cor_staticards()
    })

    # scatter plot ------------------------------------------------------------
    scatter_plot <- reactive({
      req(input$sel_var1)
      req(input$sel_var2)

      ggplot(df_active(), aes(x = .data[[input$sel_var1]],
                              y = .data[[input$sel_var2]])) +
        geom_point(color = session$userData$conf$plot_fill_color,
                   pch = if(nrow(df_active()) > 1e4) '.' else 20) +
        labs(x = input$sel_var1, y = input$sel_var2) +
        theme_classic() +
        theme(axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16)
        )

    }) |> bindEvent(input$btn_scatter)

    output$scatter_plot <- renderPlot({
      req(scatter_plot())
      scatter_plot()
    }, res = 96)

    # insert scatter to output --------------------------------------------------------
    mod_output_scatter <- insert_output_server(
      'insert_scatter',
      reactive(plotTag(scatter_plot(), '', width = 1000, height = 500)))

    # get return from insert output module ------------------------------------
    observe({
      req(mod_output_scatter$output_element())

      output_list$elements[[gen_element_id()]] <- mod_output_scatter$output_element()

    }) |> bindEvent(mod_output_scatter$output_element())

    # help events -------------------------------------------------------------
    observe({
      showModal(modalDialog(
        HTML(get_help_file('stats', 'cor.test')),
        easyClose = TRUE, size = 'xl'
      ))
    }) |> bindEvent(input$btn_help_cor)

    # update output -----------------------------------------------------------
    observe({
      session$userData$out$elements <- output_list$elements
    })

  })
}
