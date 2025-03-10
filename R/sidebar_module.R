
# ui --------------------------------------------------------------------------
sidebar_ui <- function(id) {
  ns <- NS(id)

  sidebar(
    open = F,
    accordion(
      open = T,
      accordion_panel(
        class = 'accordion-sidebar',
        'Dataset Info',
        icon = bs_icon('file-binary', size = '1.75em'),
        uiOutput(ns('df_info'))
      )))
}

# server ----------------------------------------------------------------------
sidebar_server <- function(id, input_metadata, app_session) {
  moduleServer(id, function(input, output, session) {
  	ns <- session$ns

    output$df_info <- renderUI({
      tagList(
        h5(input_metadata()$name),
        p('Rows/Columns:',
          paste(input_metadata()$nrow |> f_num(dig = 1),
                '/', input_metadata()$ncol |> f_num())
        ),
        p("Columns with NA's:", input_metadata()$n_nas),
        p('Size (MB):', (input_metadata()$size)),
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

  })
}
