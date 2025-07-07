
# ui --------------------------------------------------------------------------
metadata_ui <- function(id) {
  ns <- NS(id)
  card(
    card_body(gt_output(ns('metadata_gt'))),
    card_footer(insert_output_ui(ns('metadata_insert_output')))
  )
}

# server ----------------------------------------------------------------------
metadata_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # metadata ----------------------------------------------------------------
    act_meta_gt <- reactive({
      req(session$userData$df$act_meta())
      gt_info(session$userData$df$act_meta(), session$userData$df$act_name)
    })

    output$metadata_gt <- render_gt({
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

    # output ------------------------------------------------------------------
    output_list <- reactiveValues(elements = NULL)

    observe({
      output_list$elements <- session$userData$out$elements
    })

    # insert to output module -------------------------------------------------
    mod_insert_output <- insert_output_server(
      'metadata_insert_output',
      reactive(act_meta_gt() |> tab_options(table.width = pct(90)))
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
