
# ui --------------------------------------------------------------------------
lm_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Linear Model', class = 'mini-header'),
    layout_sidebar(
      class = 'card-sidebar',
      sidebar = sidebar(
        # uiOutput(ns('parameters'))
        selectInput(ns('sel_yvar'), 'Dependent Variable', NULL),
        selectizeInput(ns('sel_xvar'), 'Independent Variables', NULL,
                       multiple = T,
                       options = list(plugins = list('remove_button', 'clear_button')))
        ),
      navset_card_pill(
        nav_panel(
          'Model',
          card(
            layout_sidebar(
              sidebar = sidebar(
                width = 400,
                layout_columns(
                  col_widths = c(6, 6),
                  btn_task(ns('btn_run_lm'), 'Run Model', icon('gear')),
                  btn_task(ns('btn_help_lm'), 'Help', icon('question'))
                )
              ),
              card_body(
                gt_output(ns('lm_var_table')),
                gt_output(ns('lm_metrics'))
              ),
              card_footer(
                uiOutput(ns('conditional_add_output'))
              )
            )
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
lm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  df <- reactive({ session$userData$df$act })

    # outupt objects ----------------------------------------------------------
	  output_list <- reactiveValues(elements = NULL)

	  observe({
	    output_list$elements <- session$userData$out$elements
	  })

    var_analysis <- reactive({
      session$userData$df$act_meta() |> filter(perc_nas != 1) |> pull(var)
    })

    yvar <- reactive({
      req(var_analysis())
      intersect(var_analysis(), names(df())[sapply(df(), is.numeric)])
    })

    xvar <- reactive({
      req(yvar())
      var_analysis()[var_analysis() %notin% input$sel_yvar]
    })

    observe({
      req(yvar())
      updateSelectInput(session, 'sel_yvar', choices = yvar())
    })

    observe({
      req(xvar())
      updateSelectizeInput(session, 'sel_xvar', choices = xvar())
    })

    # linear model ------------------------------------------------------------
    linear_model <- reactiveValues(
      model = NULL,
      summary = NULL,
      x = NULL,
      y = NULL,
      x_name = '',
      y_name = ''
    )

    observe({
      req(input$sel_yvar, input$sel_xvar)

      linear_model$y_name <- input$sel_yvar
      linear_model$x_name <- paste(input$sel_xvar, collapse = '+')

      form <- formula(paste(linear_model$y_name, '~', linear_model$x_name))

      linear_model$model <- lm(form, data = df(), model = F)

      linear_model$summary <- summary(linear_model$model)

      msg('Lm model completed.', DURATION = 0.5)

    }) |> bindEvent(input$btn_run_lm)


    # linear model output -----------------------------------------------------
    lm_var_table <- reactive({
      req(linear_model$model)

      table_summary <- as.data.frame(linear_model$summary$coefficients)

      table_summary$Variable <- rownames(table_summary)
      rownames(table_summary) <- NULL

      table_summary <- table_summary[, c("Variable", "Estimate", "Std. Error",
                                         "t value", "Pr(>|t|)")]

      table_summary <- table_summary |> as.data.table()
      table_summary[, `Sig Levels` := fcase(
        `Pr(>|t|)` < 0.001, '***',
        `Pr(>|t|)` < 0.01, '**',
        `Pr(>|t|)` < 0.05, '*',
        `Pr(>|t|)` < 0.1, '.',
        default = ''
      )]

      table_summary |> gt() |>
        tab_header(title = 'Linear Model',
                   subtitle = paste('Independent Variable:', linear_model$y_name))

    })

    lm_metrics <- reactive({
      req(linear_model$model)

      data.frame(
        Metric = c('R-squared', 'Adjusted R-squared', 'F-statistic', 'F p-value'),
        Value = c(linear_model$summary$r.squared,
                  linear_model$summary$adj.r.squared,
                  linear_model$summary$fstatistic[1],
                  linear_model$summary$fstatistic[3])
      ) |> gt() |> tab_header('Model metrics')
    })

    output$lm_var_table <- render_gt({
      req(lm_var_table())
      lm_var_table()
    })

    output$lm_metrics <- render_gt({
      req(lm_metrics())
      lm_metrics()
    })

    # help events -------------------------------------------------------------
    observe({
      showModal(modalDialog(
        HTML(get_help_file('stats', 'lm')),
        easyClose = TRUE, size = 'xl'
      ))
    }) |> bindEvent(input$btn_help_lm)

    # insert model to output --------------------------------------------------
    mod_insert_output_model <- insert_output_server(
      'lm_insert_output',
      reactive(gen_table2(lm_var_table(), lm_metrics()))
    )

    output$conditional_add_output <- renderUI({
      req(linear_model$model)
      insert_output_ui(ns('lm_insert_output'))
    })

    # get return from insert output module ------------------------------------
    observe({
      req(mod_insert_output_model$output_element())

      output_list$elements[[gen_element_id()]] <- mod_insert_output_model$output_element()

    }) |> bindEvent(mod_insert_output_model$output_element())

    # update output -----------------------------------------------------------
    observe({
      session$userData$out$elements <- output_list$elements
    })

  })
}
