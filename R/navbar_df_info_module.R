
# ui --------------------------------------------------------------------------
navbar_df_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('navbar_df_info')),
    actionButton(ns('df_btn_overview'), '', icon('magnifying-glass'), class = 'mini-btn'),
    actionButton(ns('df_btn_meta'), '', bs_icon('info-circle'), class = 'mini-btn'),
    actionButton(ns('df_btn_change'), '', icon('shuffle'), class = 'mini-btn'),
    actionButton(ns('df_btn_explore'), '', icon('chart-simple'), class = 'mini-btn')
  )
}

# server ----------------------------------------------------------------------
navbar_df_info_server <- function(id, app_session) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    output$navbar_df_info <- renderUI({
      req(session$userData$dt$act_mini_meta())

      mini_meta <- session$userData$dt$act_mini_meta()
      tagList(
        p('Rows/Columns:', mini_meta[['row_col']]),
        p("Columns with NA's:", mini_meta[['col_nas']]),
        p('Size (MB):', mini_meta[['size']])
      )
    })

    # buttons --------
    observe({
      nav_select('navbar', selected = 'Data', session = app_session)
      nav_select('navset_card_pill_data', selected = 'Overview', session = app_session)
    }) |> bindEvent(input$df_btn_overview)

    observe({
      nav_select('navbar', selected = 'Data', session = app_session)
      nav_select('navset_card_pill_data', selected = 'Metadata', session = app_session)
    }) |> bindEvent(input$df_btn_meta)

    observe({
      nav_select('navbar', selected = 'Data', session = app_session)
      nav_select('navset_card_pill_data', selected = 'Data', session = app_session)
    }) |> bindEvent(input$df_btn_change)

    observe({
      nav_select('navbar', selected = 'Exploratory', session = app_session)
    }) |> bindEvent(input$df_btn_explore)

  })
}
