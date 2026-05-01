
# ui --------------------------------------------------------------------------
about_spada_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    value = 'about',
    title = 'About',
    icon = icon('info'),
    card(
      class = 'big-card',
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_body(
            div(
              style = 'display:flex; align-items:center; gap:12px;',
              tags$img(src = 'spada/favicon-32x32.png'),
              h4('Package Description', style = 'margin:0;')
            ),
            verbatimTextOutput(ns('info_about_spada'))
          )
        ),
        card(
          card_body(
            h4(icon('r-project'), 'Session Info'),
            verbatimTextOutput(ns('session_info'))
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
about_spada_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    output$session_info <- renderPrint(
      sessionInfo()
    )

    output$info_about_spada <- renderPrint(
      packageDescription('spada')
    )
  })
}
