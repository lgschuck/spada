
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
stats_table_server <- function(id, var1, var2, input_percentile, percentile,
                               var1_sd, pearson_correlation) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    stats_obs <- reactive({
      req(var1())
      length(var1())
    })

    stats_n_nas <- reactive({
      req(var1())
      whichNA(var1()) |> NROW()
    })

    stats_min <- reactive({
      req(var1())
      if(is.numeric(var1())) fmin(var1(), na.rm = T) else NA
    })

    stats_q1 <- reactive({
      req(var1())
      if(is.numeric(var1())) fquantile(var1(), 0.25) else NA
    })

    stats_median <- reactive({
      req(var1())
      if(is.numeric(var1())) fmedian(var1(), na.rm = T) else NA
    })

    stats_mean <- reactive({
      req(var1())
      if(is.numeric(var1())) fmean(var1(), na.rm = T) else NA
    })

    stats_mode <- reactive({
      req(var1())
      if(var1() |> is.numeric() || var1() |> is.character() ||
          var1() |> is.factor()){
        fmode(var1(), na.rm = T)
      } else {
        NA
      }
    })

    stats_q3 <- reactive({
      req(var1())
      if(is.numeric(var1())) fquantile(var1(), 0.75) else NA
    })

    stats_max <- reactive({
      req(var1())
      if(is.numeric(var1())) fmax(var1(), na.rm = T) else NA
    })

    # table -------------------------------------------------------------------
    stats_table <- reactive({
      req(var1(), input_percentile())

      fmt_digits <- min(max(0, input$table_digits), 9)

      data.frame(
        measure = c(
          paste("% NA's (", stats_n_nas(), '/', stats_obs(), ')'),
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
          (stats_n_nas() / stats_obs() * 100) |> f_num(dig = fmt_digits),
          stats_min() |> f_num(dig = fmt_digits),
          stats_q1() |> f_num(dig = fmt_digits),
          stats_median() |> f_num(dig = fmt_digits),
          stats_mean() |> f_num(dig = fmt_digits),
          if(stats_mode() |> allNA()) NA else paste(
            stats_mode()|> f_num(dig = fmt_digits), collapse = ' | '),
          stats_q3() |> f_num(dig = fmt_digits),
          stats_max() |> f_num(dig = fmt_digits),
          percentile() |> f_num(dig = fmt_digits),
          var1_sd() |> f_num(dig = fmt_digits),
          pearson_correlation() |> f_num(dig = fmt_digits)
        )
      )
    })

    # format tabe -------------------------------------------------------------
    stats_table_fmt <- reactive({
      req(stats_table())
      stats_table() |>
        gt() |>
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
