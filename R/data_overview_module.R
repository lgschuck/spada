
# ui --------------------------------------------------------------------------
data_overview_ui <- function(id) {
  ns <- NS(id)
  card(
    card_body(
      selectInput(
        inputId = ns('sel_dataset'),
        label = 'Dataset',
        choices = NULL
      ),
      gt_output(ns('gt'))
    ),
    card_footer(
      fluidRow(
        column(2, numericInput(ns('size_sample'), 'Number of rows', 500, 0, 1e4, 500)),
        column(2, radioGroupButtons(
          ns('radio_sample'), 'Show',
          c('First rows' = 'first', 'Last rows' = 'last', 'Sample' = 'sample'),
          size = 'sm', individual = T)),
        column(2, div(
          insert_output_ui(ns('data_overview_insert_output'))),
          style = 'margin-top: 28px'
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
data_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(session$userData$dt$df_info())

      updateSelectInput(
        session,
        inputId = 'sel_dataset',
        choices = c(
          session$userData$dt$act_name,
          setdiff(session$userData$dt_names(), session$userData$dt$act_name)
        )
      )
    })

    df <- reactive({
      req(session$userData$dt$dt, input$sel_dataset)

      session$userData$dt$dt[[input$sel_dataset]]
    })

    idx <- reactive({
      req(df(), input$size_sample)

      validate(need(input$size_sample > 0, 'Number of rows must be > 0'))

      n_show <- max(1, input$size_sample)
      n_show <- min(n_show, nrow(df()))

      if(input$radio_sample == 'first'){
        idx <- 1:n_show
      } else if(input$radio_sample == 'last'){
        start_idx <- max(1, nrow(df()) - n_show + 1)
        idx <- start_idx:nrow(df())
      } else if (input$radio_sample == 'sample'){
        idx <- sample.int(nrow(df()), n_show, replace = F)
      }
    })

    data_filtered <- reactive({
      req(idx())

      df()[idx(), ] |>
        lapply(\(x) if(is.complex(x)) as.character(x) else x) |>
        as.data.frame()
    })

    data_gt <- reactive({
      req(data_filtered())
      data_filtered() |>
        gt() |>
        tab_header(input$sel_dataset)
    })

    output$gt <- render_gt({
      req(data_gt())

      data_gt() |>
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

    # output ------------------------------------------------------------------
    output_list <- reactiveValues(elements = NULL)

    observe({
      output_list$elements <- session$userData$out$elements
    })

    # insert to output module -------------------------------------------------
    mod_insert_output <- insert_output_server(
      'data_overview_insert_output',
      reactive(data_gt() |> tab_options(table.width = pct(90)))
    )

    # get return from insert output module ------------------------------------
    observe({
      req(mod_insert_output$output_element())

      output_list$elements[[gen_element_id()]] <- mod_insert_output$output_element()

    }) |> bindEvent(mod_insert_output$output_element())

    # update output -----------------------------------------------------------
    observe({
      session$userData$out$elements <- output_list$elements
    })

  })
}
