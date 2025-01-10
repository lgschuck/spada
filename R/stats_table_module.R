
# ui --------------------------------------------------------------------------
stats_table_ui <- function(id) {
  ns <- NS(id)
  card(
    card_body(gt_output(ns('gt_stats'))),
    card_footer(numericInput(ns('table_digits'), 'Digits', 2, 0, 9, 1))
  )
}

# server ----------------------------------------------------------------------
stats_table_server <- function(id, var1, var2, input_percentile, percentile,
                               var1_sd, pearson_correlation) {
  moduleServer(id, function(input, output, session) {

    stats_obs <- reactive(length(var1()))
    stats_n_nas <- reactive(sum(is.na(var1())))
    stats_min <- reactive(if(is.numeric(var1())) mina(var1()) else NA)
    stats_q1 <- reactive(if(is.numeric(var1())) p25(var1()) else NA)
    stats_median <- reactive(if(is.numeric(var1())) median(var1(), na.rm = T) else NA)
    stats_mean <- reactive(if(is.numeric(var1())) mean(var1(), na.rm = T) else NA)
    stats_q3 <- reactive(if(is.numeric(var1())) p75(var1()) else NA)
    stats_max <- reactive(if(is.numeric(var1())) mana(var1()) else NA)

    stats_table <- reactive(
      data.frame(
        var = c(
          paste("% NA's (", stats_n_nas(), '/', stats_obs(), ')'),
          'Minimum',
          'Percentile 25',
          'Median',
          'Mean',
          'Percentile 75',
          'Maximum',
          paste('Percentile', input_percentile()),
          'Standard Deviation',
          'Pearson Correlation'
        ),
        value = c(
          stats_n_nas() / stats_obs() * 100,
          stats_min(),
          stats_q1(),
          stats_median(),
          stats_mean(),
          stats_q3(),
          stats_max(),
          percentile(),
          var1_sd(),
          pearson_correlation()
        )
      )
    )

    output$gt_stats <- render_gt({
      validate(
        need(
          isTruthy(input_percentile()) &&
            between(input_percentile(), 0, 100),
          'Percentile must be between 0 and 100'))

      stats_table() |>
        gt() |>
        sub_missing() |>
        cols_label(var = 'Measure', value = 'Value') |>
        fmt_number(decimals = input$table_digits) |>
        opt_interactive(use_pagination = F,
                        use_highlight = T,
                        use_compact_mode = T) |>
        tab_options(table.background.color = '#ffffff')
    }) |> bindCache(
      input$table_digits,
      stats_table()
    )

  })
}
