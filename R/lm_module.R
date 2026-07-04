
# ui --------------------------------------------------------------------------
lm_ui <- function(id) {

  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Linear Model', class = 'mini-header'),
    layout_sidebar(
      class = 'card-sidebar',
      sidebar = sidebar(
        selectInput(ns('sel_yvar'), 'Dependent Variable', character(0)),
        selectizeInput(ns('sel_xvar'), 'Independent Variables', character(0),
                       multiple = T,
                       options = list(plugins = list('remove_button', 'clear_button'))),
        layout_columns(
          col_widths = c(8),
          btn_task(ns('btn_run_lm'), 'Run Model', icon('gear'))
        ),
        layout_columns(
          col_widths = c(8),
          btn_task(ns('btn_help_lm'), 'Help', icon('question'))
        )
      ),
      navset_card_pill(
        nav_panel(
          'Output',
          card(
            card_body(
              gt_output(ns('lm_var_table')),
              gt_output(ns('lm_metrics'))
            ),
            card_footer(
              uiOutput(ns('conditional_add_output')),
              uiOutput(ns('conditional_save_model'))
            )
          )
        ),
        nav_panel(
          'Residuals',
          plotOutput(ns('lm_resid_plot')),
          card_footer(
            fluidRow(
              column(
                2,
                radioGroupButtons(
                  ns('radio_lm_resid'),
                  'Plot type:',
                  c(
                    'Histogram' = 'hist',
                    'Boxplot' = 'boxplot',
                    'Dots' = 'dots'
                  ),
                  size = 'sm',
                  individual = T
                )
              ),
              column(
                2,
                btn_task(ns('btn_lm_resid'),
                         'Plot residuals',
                         icon('chart-simple'),
                         style = 'margin-top: 28px')
              ),
              column(
                2,
                div(insert_output_ui(ns('insert_lm_resid_plot')), style = 'margin-top: 28px')
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

	  df <- reactive(get_act_dt(session))

    var_analysis <- reactive({
      session$userData$dt$act_meta()[perc_nas != 1, var]
    })

    yvar <- reactive({
      req(var_analysis())
      intersect(var_analysis(), names(df())[sapply(df(), is.numeric)])
    })

    xvar <- reactive({
      req(input$sel_yvar)
      if(length(yvar()) > 0){
        var_analysis()[var_analysis() %notin% input$sel_yvar]
      } else {
        character(0)
      }
    })

    observe({
      updateSelectInput(session, 'sel_yvar', choices = yvar(), selected = yvar()[1])
    })

    observe({
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
      if (!isTruthy(input$sel_yvar) || !isTruthy(input$sel_xvar)) {
        msg('Select dependent and independent variables')
        return()
      }

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
      linear_model_df_output(linear_model$summary) |>
        gt() |>
        tab_header(title = 'Linear Model',
                   subtitle = paste('Dependent Variable:', linear_model$y_name))
    })

    lm_metrics <- reactive({
      req(linear_model$model)
      linear_model_df_metrics(linear_model$summary) |>
        gt() |> tab_header('Model metrics')
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
      fun_help_modal('stats', 'lm')
    }) |> bindEvent(input$btn_help_lm)

    # insert model to output --------------------------------------------------
    insert_output_server(
      'lm_insert_output',
      reactive(gen_table2(lm_var_table(), lm_metrics())),
      'Linear Model'
    )

    output$conditional_add_output <- renderUI({
      req(linear_model$model)
      insert_output_ui(ns('lm_insert_output'))
    })

    # save model object -------------------------------------------------------
    output$conditional_save_model <- renderUI({
      req(linear_model$model)
      actionButton(ns('btn_save_model'), 'Save Model', icon('download'), class = 'btn-task')
    })

    observe({
      showModal(
        modalDialog(
          title = div(icon('download'), 'Save Model'),
          h5('This will save the Model Output and the Metrics tables.'),
          textInput(ns('model_filename'), 'File name', value = 'model'),
          footer = tagList(
            actionButton(ns('btn_close_save_model'), 'Close', icon('xmark'),
                     class = 'btn-task btn-task-cancel'),
            downloadButton(ns('down_handler'), 'Save model',
                           class = 'btn-task', icon = icon('download'))
          ),
          size = 'l'
        )
      )
    }) |> bindEvent(input$btn_save_model)

    observe({
      removeModal()
    }) |> bindEvent(input$btn_close_save_model)

    # download handler for the model file ----
    output$down_handler <- downloadHandler(

      filename = function() {
        paste0(input$model_filename, '.RDS')
      },
      content = function(file) {
        saveRDS(
          list(
            'model' = linear_model_df_output(linear_model$summary),
            'metrics' = linear_model_df_metrics(linear_model$summary)
          ),
          file,
          compress = F
        )
      }
    )

    # plot linear model residuals ---------------------------------------------
    update_lm_resid_plot <- reactiveVal(0)

    observe({
      # print('teste')

      # cat("antes:", update_lm_resid_plot(), "\n")
      update_lm_resid_plot(update_lm_resid_plot() + 1)
      # cat("depois:", update_lm_resid_plot(), "\n")
    }) |> bindEvent(input$btn_lm_resid)

    lm_resid_plot <- reactive({
      req(linear_model$model$residuals)
      req(update_lm_resid_plot() > 0)

      if(input$radio_lm_resid == 'hist'){

        spada_plot(type = 'hist',
                   df = data.frame(x = linear_model$model$residuals),
                   xvar = 'x',
                   ylab = 'Count',
                   fill_color = session$userData$conf$plot_fill_color,
                   line_color = session$userData$conf$plot_line_color,
                   title_color = session$userData$conf$plot_title_color,
                   title = 'Linear Model - Residuals',
                   sample_limit = session$userData$conf$plot_limit
        )

      } else if (input$radio_lm_resid == 'boxplot'){

        spada_plot(type = 'boxplot',
                   df = data.frame(x = linear_model$model$residuals),
                   xvar = 'x',
                   fill_color = session$userData$conf$plot_fill_color,
                   line_color = session$userData$conf$plot_line_color,
                   title_color = session$userData$conf$plot_title_color,
                   title = 'Linear Model - Residuals',
                   sample_limit = session$userData$conf$plot_limit
        )

      } else if (input$radio_lm_resid == 'dots'){

        spada_plot(type = 'dots',
                   df = data.frame(x = seq_along(linear_model$model$residuals),
                                   y = linear_model$model$residuals),
                   xvar = 'x',
                   yvar = 'y',
                   xlab = 'Index',
                   ylab = 'Values',
                   fill_color = session$userData$conf$plot_fill_color,
                   line_color = session$userData$conf$plot_line_color,
                   title_color = session$userData$conf$plot_title_color,
                   title = 'Linear Model - Residuals',
                   vertical_line = 0,
                   point_shape = if(session$userData$conf$plot_limit > 1e4 &&
                                    length(linear_model$model$residuals) > 1e4) '.' else 20,
                   sample_limit = session$userData$conf$plot_limit,
                   line_type = 2
        )
      }
    }) |> bindEvent(update_lm_resid_plot())

    output$lm_resid_plot <- renderPlot({
      validate(need(isTruthy(linear_model$model), 'No residuals to plot'))

      lm_resid_plot()
    }, res = 96)

    # insert lm residual plot to output ---------------------------------------
    insert_output_server(
      'insert_lm_resid_plot',
      reactive(plot_tag(lm_resid_plot())),
      'Linear Model - Residuals Plot'
    )

  })
}
