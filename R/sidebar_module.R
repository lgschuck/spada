
# ui --------------------------------------------------------------------------
sidebar_ui <- function(id) {
  ns <- NS(id)

  sidebar(
    open = T,
    accordion(
      open = T,
      accordion_panel(
        class = 'accordion-sidebar',
        'Active Dataset',
        icon = bs_icon('check2-square', size = '1.75em'),
        uiOutput(ns('df_info')),
        fluidRow(
          column(1),
          column(2, actionButton(ns('df_btn_overview'), '',
                                 icon('magnifying-glass'), class = 'mini-btn') |>
                   tooltip('Overview', placement = 'bottom')),
          column(2, actionButton(ns('df_btn_change'), '', icon('shuffle'),
                                 class = 'mini-btn') |>
                   tooltip('Change dataset', placement = 'bottom')),
          column(2, actionButton(ns('df_btn_explore'), '', icon('chart-simple'),
                                 class = 'mini-btn') |>
                   tooltip('Exploratory Analysis', placement = 'bottom')),
        )
      )
    ),
    accordion(
      open = T,
      accordion_panel(
        class = 'accordion-sidebar',
        'Datasets',
        icon = bs_icon('stack', size = '1.75em'),
        selectInput(ns('sel_datasets_names'), '', choices = NULL),
        actionButton(ns('btn_preview_dt'), 'Preview', icon('magnifying-glass'),
                     class = 'mini-btn') |>
          popover(htmlOutput(ns('df_preview')),
                  options = list(customClass = 'preview-dt-popup'))
      )
    )
  )
}

# server ----------------------------------------------------------------------
sidebar_server <- function(id, app_session) {
  moduleServer(id, function(input, output, session) {
  	ns <- session$ns

  	# active dataset info -----------------------------------------------------
    output$df_info <- renderUI({
      tagList(
        h5(session$userData$dt$act_name),
        p('Rows/Columns:', session$userData$dt$act_row_col()),
        p("Columns with NA's:", session$userData$dt$act_col_nas()),
        p('Size (MB):', session$userData$dt$act_size())
      )
    })

    observe({
      nav_select('navbar', selected = 'Data', session = app_session)
      nav_select('navset_card_pill_data', selected = 'Overview', session = app_session)
    }) |> bindEvent(input$df_btn_overview)

    observe({
      nav_select('navbar', selected = 'Data', session = app_session)
      nav_select('navset_card_pill_data', selected = 'Data', session = app_session)
    }) |> bindEvent(input$df_btn_change)

    observe({
      nav_select('navbar', selected = 'Analysis', session = app_session)
      nav_select('navbar', selected = 'Exploratory', session = app_session)
    }) |> bindEvent(input$df_btn_explore)

    # list of datasets --------------------------------------------------------
    observe({
      req(session$userData$dt$dt, session$userData$dt$act_name)
      choices <- c(
        session$userData$dt$act_name,
        setdiff(session$userData$dt_names(), session$userData$dt$act_name)
      )

      updateSelectInput(session, 'sel_datasets_names', choices = choices)
    })

    # preview dataset -------------------------
    output$df_preview <- renderUI({
      req(session$userData$dt$dt, input$sel_datasets_names)

      df <- session$userData$dt$dt[[input$sel_datasets_names]][1:5, ]

      df <- lapply(df, make_valid_cols) |> as.data.frame()

      gt::gt(df) |>
        tab_header(input$sel_datasets_names) |>
        tab_options(heading.align = 'left') |>
        gt::as_raw_html() |>
        HTML()
    })

  })
}
