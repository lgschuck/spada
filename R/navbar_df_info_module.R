
# ui --------------------------------------------------------------------------
navbar_df_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('navbar_df_info')),
    actionButton(ns('df_btn_overview'), '', icon('magnifying-glass'), class = 'mini-btn'),
    actionButton(ns('df_btn_change'), '', icon('shuffle'), class = 'mini-btn'),
    actionButton(ns('df_btn_explore'), '', icon('chart-simple'), class = 'mini-btn')
  )
}

# server ----------------------------------------------------------------------
navbar_df_info_server <- function(id, app_session) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    output$navbar_df_info <- renderUI({
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
      nav_select('navbar', selected = 'Exploratory', session = app_session)
    }) |> bindEvent(input$df_btn_explore)

  })
}
