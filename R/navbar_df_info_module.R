
# ui --------------------------------------------------------------------------
navbar_df_info_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('navbar_df_info'))
}

# server ----------------------------------------------------------------------
navbar_df_info_server <- function(id, app_session) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    output$navbar_df_info <- renderUI({
      tagList(
        h5(session$userData$dt$act_name),
        p('Rows/Columns:',
          paste(session$userData$dt$act_meta() |> pull(rows) |> head(1) |> f_num(dig = 1), '/',
                session$userData$dt$act_meta() |> pull(cols) |> head(1) |> f_num())
        ),
        p("Columns with NA's:", session$userData$dt$act_meta() |>
            filter(n_nas > 0) |>
            nrow()),
        p('Size (MB):', (object.size(get_act_dt(session)) / 2^20) |>
            as.numeric() |> round(2)),
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
