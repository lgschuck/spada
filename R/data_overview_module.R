
# ui --------------------------------------------------------------------------
data_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card_body(gt_output(ns('gt'))),
    fluidRow(
      column(2, numericInput(ns('size_sample'), 'Number of rows', 100, 100, 1e4, 100)),
      column(2, radioButtons(
        ns('radio_sample'), 'Show',
        c('First rows' = 'first', 'Sample' = 'sample'), inline = T))
    )
  )
}

# server ----------------------------------------------------------------------
data_overview_server <- function(id, df, triggers) {
  moduleServer(id, function(input, output, session) {

    output$gt <- render_gt({
        req(input$size_sample)

        validate(need(input$size_sample > 0, 'Number of rows must be > 0'))

        triggers()

        n_show <- max(1, input$size_sample)
        n_show <- min(n_show, nrow(df()))

        if(input$radio_sample == 'first'){
          idx <- 1:n_show
        } else if (input$radio_sample == 'sample'){
          idx <- sample.int(nrow(df()), n_show, replace = F)
        }

        df()[idx, ] |>
          lapply(\(x) if(is.complex(x)) as.character(x) else x) |>
          as.data.frame() |>
          gt() |>
          opt_interactive(
            page_size_default = 9,
            use_filters = T,
            use_resizers = T,
            use_highlight = T,
            use_compact_mode = T,
            use_text_wrapping = F,
            use_page_size_select = T
          ) |>
          tab_options(table.background.color = '#f9f9f9')
    })
  })
}
