
# ui --------------------------------------------------------------------------
navbar_df_info_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('navbar_df_info'))
}

# server ----------------------------------------------------------------------
navbar_df_info_server <- function(id, input_metadata, app_session) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    output$navbar_df_info <- renderUI({
      tagList(
        h5(input_metadata()$name),
        p('Rows/Columns:',
          paste(input_metadata()$nrow |> f_num(dig = 1), '/',
                input_metadata()$ncol |> f_num())
        ),
        p("Columns with NA's:", input_metadata()$n_nas),
        p('Size (MB):', (input_metadata()$size)),
        actionButton(ns('df_btn_overview'), '', icon('magnifying-glass'), class = 'mini-btn'),
        actionButton(ns('df_btn_change'), '', icon('shuffle'), class = 'mini-btn'),
        actionButton(ns('df_btn_explore'), '', icon('chart-simple'), class = 'mini-btn')
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
      nav_select('navbar', selected = 'Exploratory', session = app_session)
    }) |> bindEvent(input$df_btn_explore)

  })
}
