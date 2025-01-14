
# ui --------------------------------------------------------------------------
page_config_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    value = 'config',
    title = 'Config',
    icon = bs_icon('sliders2'),
    card(style = 'background-color: #02517d;', layout_columns(
      col_widths = c(9, 3), card(card_body(
        h4('Colors'),
        selectInput(
          ns('sel_palette'),
          'Select colors for plots',
          c(
            'Palette 1' = 1,
            'Palette 2' = 2,
            'Palette 3' = 3
          )
        ),
        plotOutput(ns('sample_plot'))
      )), card(card_body(
        h4('Size of input files'),
        numericInput(ns('input_file_size'), 'Size in MB', 500, min = 0, step = 100),
        btn_task(ns('btn_file_size'), 'Apply', icon('check'))
      ))
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

    observe({
      if(!isTruthy(input$input_file_size) || input$input_file_size < 1) {
        msg('Value must be > 1')
      } else {
        options(shiny.maxRequestSize = input$input_file_size * 1024 ^ 2)
        msg('New limit applied')
      }
    }) |> bindEvent(input$btn_file_size)

    return(list(palette = palette))
  })
}
