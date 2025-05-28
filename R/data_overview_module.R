
# ui --------------------------------------------------------------------------
data_overview_ui <- function(id) {
  ns <- NS(id)
  card(
    card_body(gt_output(ns('gt'))),
    card_footer(
      fluidRow(
        column(2, numericInput(ns('size_sample'), 'Number of rows', 500, 0, 1e4, 500)),
        column(2, radioGroupButtons(
          ns('radio_sample'), 'Show',
          c('First rows' = 'first', 'Sample' = 'sample'),
          size = 'sm', individual = T))
      )
    )
  )
}

# server ----------------------------------------------------------------------
data_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    df <- reactive(session$userData$df$act)

    idx <- reactive({
      req(input$size_sample)

      validate(need(input$size_sample > 0, 'Number of rows must be > 0'))

      n_show <- max(1, input$size_sample)
      n_show <- min(n_show, nrow(df()))

      if(input$radio_sample == 'first'){
        idx <- 1:n_show
      } else if (input$radio_sample == 'sample'){
        idx <- sample.int(nrow(df()), n_show, replace = F)
      }
    })

    data_gt <- reactive({
      req(idx())

      df()[idx(), ] |>
        lapply(\(x) if(is.complex(x)) as.character(x) else x) |>
        as.data.frame()
    })

    output$gt <- render_gt({
      req(data_gt())

      data_gt() |>
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
        tab_options(table.background.color = bg_color)
    })
  })
}
