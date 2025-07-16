
# ui --------------------------------------------------------------------------
sidebar_ui <- function(id) {
  ns <- NS(id)

  sidebar(
    open = F,
    accordion(
      open = T,
      accordion_panel(
        class = 'accordion-sidebar',
        'Active Dataset',
        icon = bs_icon('check2-square', size = '1.75em'),
        uiOutput(ns('df_info'))
      )
    ),
    accordion(
      open = F,
      accordion_panel(
        class = 'accordion-sidebar',
        'Datasets',
        icon = bs_icon('table', size = '1.75em'),
        selectInput(ns('sel_datasets_names'), '', choices = NULL),
        actionButton(ns('btn_preview_dt'), 'Preview', icon('magnifying-glass'),
                     class = 'mini-btn') |>
          popover(htmlOutput(ns('df_preview')))
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
        p('Rows/Columns:',
          paste(session$userData$dt$act_meta() |> pull(rows) |> head(1) |> f_num(dig = 1),
                '/', session$userData$dt$act_meta() |> pull(cols) |> head(1) |> f_num())
        ),
        p("Columns with NA's:", session$userData$dt$act_meta() |>
            filter(n_nas > 0) |>
            nrow()),
        p('Size (MB):', (object.size(get_act_dt(session)) / 2^20) |>
            as.numeric() |> round(2)),
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

    # review dataset -------------------------
    output$df_preview <- renderUI({
      req(session$userData$dt$dt, input$sel_datasets_names)

      df <- session$userData$dt$dt[[input$sel_datasets_names]][1:5, ]

      df <- lapply(df, \(x) if (is.complex(x))
        as.character(x)
        else
          x) |>
        as.data.frame()

      gt::gt(df) |>
        tab_header(input$sel_datasets_names) |>
        tab_options(heading.align = 'left') |>
        gt::as_raw_html() |>
        HTML()
    })

  })
}
