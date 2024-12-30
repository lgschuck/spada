
# ui --------------------------------------------------------------------------
page_config_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    value = 'config',
    title = 'Config',
    icon = bs_icon('sliders2'),
    card(style = 'background-color: #02517d;', card(
      card_body(
        h2('Config'),
        selectInput(
          ns('sel_palette'),
          'Select colors for plots',
          c(
            'Palette 1' = 1,
            'Palette 2' = 2,
            'Palette 3' = 3
          )
        ),
        fluidRow(column(3, plotOutput(
          ns('sample_plot')
        )))
      )
    ))
  )
}

# server ----------------------------------------------------------------------
page_config_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    palette <- reactive({
      if (input$sel_palette == 1) {
        return(list(
          'fill' = 'steelblue2',
          'line' = 'sienna2'
        ))
      } else if (input$sel_palette == 2) {
        return(list('fill' = 'cyan3', 'line' = 'red3'))
      } else if (input$sel_palette == 3) {
        return(list(
          'fill' = 'hotpink3',
          'line' = 'mediumseagreen'
        ))
      }
    })

    output$sample_plot <- renderPlot({
      req(palette())
      hist(
        rnorm(1e3),
        col = palette()[['fill']],
        xlab = '',
        ylab = '',
        main = ''
      )
      abline(h = 100, col = palette()[['line']], lwd = 3)

    }) |> bindCache(palette())

    return(list(palette = palette))
  })
}
