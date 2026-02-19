
# ui --------------------------------------------------------------------------
stats_table_ui <- function(id) {
  ns <- NS(id)
  card(
    card_body(gt_output(ns('gt_stats'))),
    card_footer(
      layout_columns(
        col_widths = c(6, 6),
        numericInput(ns('table_digits'), 'Digits', 2, 0, 9, 1),
        div(style = "margin-top: 28px !important;",
            save_gt_ui(ns('pA_stats_table_save_gt'))
        )
      ),
      div(style = "margin-bottom: -18px !important;")
    )
  )
}

# server ----------------------------------------------------------------------
stats_table_server <- function(id, var1, var1_name, input_percentile, percentile,
                               var1_sd, pearson_correlation) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    task_stats <- ExtendedTask$new(function(x, x_name) {
      mirai({
        list(
          var_name = x_name,

          n = length(x),

          n_nas = collapse::whichNA(x) |> NROW(),

          min = if (is.numeric(x)) collapse::fmin(x, na.rm = TRUE) else NA,

          q1 = if (is.numeric(x)) collapse::fquantile(x, 0.25) else NA,

          median = if (is.numeric(x)) collapse::fmedian(x, na.rm = TRUE) else NA,

          mean = if (is.numeric(x)) collapse::fmean(x, na.rm = TRUE) else NA,

          mode = if (is.numeric(x) || is.character(x) || is.factor(x)) {
            collapse::fmode(x, na.rm = TRUE)
          } else {
            NA
          },

          q3 = if (is.numeric(x)) collapse::fquantile(x, 0.75) else NA,

          max = if (is.numeric(x)) collapse::fmax(x, na.rm = TRUE) else NA
        )

      }, x = x, x_name = x_name)
    })

    observe({
      req(var1(), var1_name())
      task_stats$invoke(var1(), var1_name())
    }) |> bindEvent(var1())

    stats_result <- reactive({
      task_stats$result()
    })

    # table -------------------------------------------------------------------
    stats_table <- reactive({
      req(stats_result(), input_percentile())

      fmt_digits <- min(max(0, input$table_digits), 9)

      data.frame(
        measure = c(
          'Variable',
          paste("% NA's (", stats_result()$n_nas, '/', stats_result()$n, ')'),
          'Minimum',
          'Percentile 25',
          'Median',
          'Mean',
          'Mode',
          'Percentile 75',
          'Maximum',
          paste('Percentile', input_percentile()),
          'Standard Deviation',
          'Pearson Correlation'
        ),
        value = c(
          stats_result()$var_name,
          (stats_result()$n_nas / stats_result()$n * 100) |> f_num(dig = fmt_digits),
          stats_result()$min |> f_num(dig = fmt_digits),
          stats_result()$q1 |> f_num(dig = fmt_digits),
          stats_result()$median |> f_num(dig = fmt_digits),
          stats_result()$mean |> f_num(dig = fmt_digits),
          if(stats_result()$mode |> allNA()) NA else paste(
            stats_result()$mode|> f_num(dig = fmt_digits), collapse = ' | '),
          stats_result()$q3 |> f_num(dig = fmt_digits),
          stats_result()$max |> f_num(dig = fmt_digits),
          percentile() |> f_num(dig = fmt_digits),
          var1_sd() |> f_num(dig = fmt_digits),
          pearson_correlation() |> f_num(dig = fmt_digits)
        )
      )
    })

    # format tabe -------------------------------------------------------------
    stats_table_fmt <- reactive({
      req(stats_table())

      var_name <- stats_table()[1, 2]
      stats_table()[-1, ] |>
        gt() |>
        tab_header(var_name) |>
        sub_missing(missing_text = '-') |>
        sub_values(values = 'NA', replacement = '-') |>
        cols_label(measure = 'Measure', value = 'Value') |>
        cols_align('left', measure) |>
        cols_align('right', value)
    })

    # render table ------------------------------------------------------------
    output$gt_stats <- render_gt({
      req(stats_table_fmt())
      validate(
        need(
          isTruthy(input_percentile()) &&
            between(input_percentile(), 0, 100),
          'Percentile must be between 0 and 100'),
        need(
          isTruthy(input$table_digits) &&
            between(input$table_digits, 0, 9),
          'Percentile must be between 0 and 9')
        )

      stats_table_fmt() |>
        opt_interactive(use_pagination = F,
                        use_highlight = T,
                        use_compact_mode = T,
                        use_sorting = F) |>
        tab_options(table.background.color = '#ffffff')
    })

    save_gt_server('pA_stats_table_save_gt', stats_table_fmt)

    # return values -----------------------------------------------------------
    return(list(table = stats_table_fmt))
  })
}
