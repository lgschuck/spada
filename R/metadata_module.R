
# ui --------------------------------------------------------------------------
metadata_ui <- function(id) {
  ns <- NS(id)
  card(
    card_body(
      selectInput(
        inputId = ns('dataset_sel'),
        label = 'Dataset',
        choices = NULL
      ),
      gt_output(ns('gt'))
    ),
    card_footer(insert_output_ui(ns('metadata_insert_output')))
  )
}

# server ----------------------------------------------------------------------
metadata_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # metadata ----------------------------------------------------------------
    observe({
      req(session$userData$dt$gt_info())

      updateSelectInput(
        session,
        inputId = 'dataset_sel',
        choices = c(
          session$userData$dt$act_name,
          setdiff(
            names(session$userData$dt$df_info()),
            session$userData$dt$act_name
          )
        )
      )
    })

    output$gt <- render_gt({
      req(act_meta_gt())

      act_meta_gt() |>
        opt_interactive(
          use_filters = T,
          use_resizers = T,
          use_highlight = T,
          use_compact_mode = T,
          use_text_wrapping = F,
          use_page_size_select = T
        )
    })

    act_meta_gt <- reactive({
      req(session$userData$dt$gt_info(), input$dataset_sel)
      session$userData$dt$gt_info()[[input$dataset_sel]]
    })

    # insert to output module -------------------------------------------------
    mod_insert_output <- insert_output_server(
      'metadata_insert_output',
      reactive(act_meta_gt() |> tab_options(table.width = pct(90)))
    )

    # get return from insert output module ------------------------------------
    observe({
      req(mod_insert_output$output_element())

      session$userData$out$elements[[gen_element_id()]] <- mod_insert_output$output_element()

    }) |> bindEvent(mod_insert_output$output_element())

  })
}
