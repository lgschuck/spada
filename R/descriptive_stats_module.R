
# ui --------------------------------------------------------------------------
descriptive_stats_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Descriptive Statistics', class = 'mini-header'),
    layout_sidebar(
      class = 'card-sidebar',
      sidebar = sidebar(
        uiOutput(ns('parameters')),
        checkboxGroupInput(
          ns('xg_central_tendency'),
          h6('Central Tendency'), inline = T,
          c('Mean' = 'mean', 'Geometric Mean' = 'gmean', 'Harmonic Mean' = 'hmean',
            'Median' = 'median', 'Mode' = 'mode'),
          c('mean', 'gmean', 'hmean', 'median', 'mode')
        ),
        checkboxGroupInput(
          ns('xg_dispersion'), h6('Dispersion'), inline = T,
          c('Minimum' = 'min', 'Maximum' = 'max', 'IQR',
            'Range' = 'range', 'Variance' = 'var',
            'Standard Deviation' = 'sd'),
          c('min', 'max', 'IQR', 'range', 'var', 'sd')
        ),
        checkboxGroupInput(
          ns('xg_shape'), h6('Shape'), inline = T,
          c('Skewness' = 'skew', 'Kurtosis' = 'kurt'), c('skew', 'kurt')
        ),
        numericInput(ns('table_digits'), 'Digits', 2, 0, 9, 1),
        btn_task(ns('btn_stats'), 'Generate Table', icon('gear'))
      ),
      navset_card_pill(
        nav_panel(
          'Stats',
          card(full_screen = T,
            card_body(gt_output(ns('gt_stats'))),
            card_footer(
              layout_columns(
                col_widths = c(3, 3),
                uiOutput(ns('conditional_add_output')),
                uiOutput(ns('conditional_save_gt'))
              )
            )
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
descriptive_stats_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  dt <- reactive(get_act_dt(session))

    var_analysis <- reactive({ dt() |> names() })

    output$parameters <- renderUI({
      tagList(
        selectizeInput(
          ns('sel_var'), 'Variables', var_analysis(), var_analysis()[1],
          multiple = T,
          options = list(plugins = list('remove_button', 'clear_button'))
        ),
      )
    })

    # calculate stats ---------------------------------------------------------
    task_desc_stats <- ExtendedTask$new(
      function(df, fmt_digits, central_tendency, dispersion, shape){
      mirai({

        desc_stats(
          df = df,
          fmt_digits = fmt_digits,
          central_tendency = central_tendency,
          dispersion = dispersion,
          shape = shape
        )
      },
      df = df,
      fmt_digits = fmt_digits,
      central_tendency = central_tendency,
      dispersion = dispersion,
      shape = shape
      )
    }) |> bind_task_button('btn_freq_table')

    observe({
      if(!isTruthy(input$sel_var)) {
        msg('Select at least 1 variable')
        return()
      } else if(!isTruthy(input$table_digits) || input$table_digits < 1) {
        msg('The precision digits must be between 1 and 9')
        return()
      } else if(all(!isTruthy(input$xg_central_tendency),
                    !isTruthy(input$xg_dispersion),
                    !isTruthy(input$xg_shape))
                ) {
        msg('Select at least one measure')
        return()
      }

      task_desc_stats$invoke(
        df = dt()[, .SD, .SDcols = input$sel_var],
        fmt_digits = min(max(0, input$table_digits), 9),
        central_tendency = input$xg_central_tendency,
        dispersion = input$xg_dispersion,
        shape = input$xg_shape
      )
    }) |> bindEvent(input$btn_stats)

    calculated_stats <- reactive({ task_desc_stats$result() })

    # gt table ----------------------------------------------------------------
    gt_stats <- reactive({
      req(calculated_stats())
      data.frame(
        Measures = names(calculated_stats()),
        do.call(rbind, calculated_stats())
      ) |>
        gt() |>
        cols_align(align = 'right') |>
        cols_align('left', Measures) |>
        sub_missing(missing_text = '-') |>
        sub_values(values = 'NA', replacement = '-') |>
        sub_values(values = 'Gmean', replacement = 'Geometric Mean') |>
        sub_values(values = 'Hmean', replacement = 'Harmonic Mean') |>
        tab_header('Descriptive Statistics')
    })

    output$gt_stats <- render_gt({
      req(gt_stats())
      gt_stats() |>
        opt_interactive(page_size_default = 25)
    })

    # save table module -------------------------------------------------------
    save_gt_server('pA_desciptive_stats_save_gt', gt_stats)

    output$conditional_save_gt <- renderUI({
      req(gt_stats())
      save_gt_ui(ns('pA_desciptive_stats_save_gt'))
    })

    # insert to output --------------------------------------------------------
    insert_output_server('desc_stats_insert_output', gt_stats, 'Descriptive Statistics')

    output$conditional_add_output <- renderUI({
      req(gt_stats())
      insert_output_ui(ns('desc_stats_insert_output'))
    })
  })
}
