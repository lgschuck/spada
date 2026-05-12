
# ui --------------------------------------------------------------------------
frequencies_ui <- function(id) {
  ns <- NS(id)

  tagList(
    card_body(
      gt_output(ns('table')),
    ),
    card_footer(
      fluidRow(
        column(3, btn_task(ns('btn_freq'), 'Generate Table', icon('gear'))),
        column(3, insert_output_ui(ns('insert_frequencies')))
      )
    )
  )
}

# server ----------------------------------------------------------------------
frequencies_server <- function(id, var, var_name) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    freq_table <- reactive({
      req(var(), var_name())

      var <- var()
      var_name <- var_name()

      if(is_date(var)) var <- as.factor(var)

      freq_df <- Freq(var, useNA = 'always') |> as.data.frame()
      attr(freq_df, 'title') <- var_name
      freq_df

    })|> bindEvent(input$btn_freq)

    freq_table_gt <- reactive({

      req(freq_table())

      freq_table() |>
        gt() |>
        tab_header(attr(freq_table(), 'title')) |>
        fmt_percent(
          columns = c('perc', 'cumperc'),
          decimals = 4
        ) |>
        cols_label(
          level = 'Level',
          freq = 'Frequency',
          perc = 'Percent',
          cumfreq = 'Cum Frequency',
          cumperc = 'Cum Percent'
        ) |>
        fmt_number(
          columns = c('freq', 'cumfreq'),
          decimals = 0,
          sep_mark = ',',
          dec_mark = '.'
        ) |> sub_missing()
    })

    output$table <- render_gt({
      req(freq_table_gt())
      freq_table_gt() |>
        opt_interactive(
          page_size_default = 10,
          use_filters = T,
          use_resizers = T,
          use_highlight = T,
          use_compact_mode = T,
          use_text_wrapping = F,
          use_page_size_select = T
        )
    })

    # insert table of values to output ----------------------------------------
    insert_output_server('insert_frequencies', freq_table_gt, 'Frequencies')

  })
}
