
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
          paste(input_metadata()$nrow |> f_num(dec = '.', big = ',', dig = 3), '/',
                input_metadata()$ncol |> f_num(dec = '.', big = ','))
        ),
        p("Columns with NA's:", input_metadata()$n_nas),
        p('Size (MB):', (input_metadata()$size)),
        btn_task(ns('df_btn_overview'), '', bs_icon('search'),
                 style = 'padding: 5px 10px;'),
        btn_task(ns('df_btn_change'), '', bs_icon('shuffle'),
                 style = 'padding: 5px 10px;'),
        btn_task(ns('df_btn_explore'), '', bs_icon('bar-chart-line'),
                 style = 'padding: 5px 10px;')
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
