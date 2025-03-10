
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
            h4('Spada - Package Description'),
            verbatimTextOutput(ns('info_about_spada'))
          )
        ),
        card(
          card_body(
            h4('Session Info'),
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
