
# ui --------------------------------------------------------------------------
page_config_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    value = 'config',
    title = 'Config',
    icon = bs_icon('sliders2'),
    card(
      class = 'big-card',
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_body(
            h4('Colors'),
            layout_columns(
              col_widths = c(3, 3, 3),
              colorPickr(
                inputId = ns('sel_fill'),
                label = 'Fill color',
                selected = '#5CACEE',
                update = 'save'
              ),
              colorPickr(
                inputId = ns('sel_line'),
                label = 'Line color',
                selected = '#EE7942',
                update = 'save'
              ),
              btn_task(ns('reset'), 'Reset', icon('rotate'),
                       style = 'margin-top: 27px !important;')
            ),
            plotOutput(ns('sample_plot'))
          )
        ),
        card(
          card_body(
            fluidRow(
              h4('Theme'),
              layout_columns(
                col_widths = c(7, 5),
                selectInput(ns('theme_choice'), NULL,
                            choices = c('Spada' = 'spada', 'Darkly Spada' = 'dark')),
                btn_task(ns('btn_theme'), 'Apply', icon('check'))
              )
            ),
            fluidRow(
              h4('Size of input files (MB)'),
              layout_columns(
                col_widths = c(7, 5),
                numericInput(
                  ns('input_file_size'),
                  NULL,
                  1000,
                  min = 0,
                  step = 500
                ),
                btn_task(ns('btn_file_size'), 'Apply', icon('check'))
              )
            )
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
page_config_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    plot_values <- rnorm(1e3)
    palette <- reactive({
      list('fill' = input$sel_fill, 'line' = input$sel_line)
    })

    output$sample_plot <- renderPlot({
      req(palette())
      hist(
        plot_values,
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

    observe({
      updateColorPickr(session = session,
                       inputId = 'sel_fill',
                       value = '#5cacee')
      updateColorPickr(session = session,
                       inputId = 'sel_line',
                       value = '#EE7942')

    }) |> bindEvent(input$reset)

    # change theme
    observe({
      session$setCurrentTheme(

        if(input$theme_choice == 'spada'){
          spada_theme
        } else if(input$theme_choice == 'dark'){
          spada_dark_theme
        }
      )
    }) |> bindEvent(input$btn_theme)

    return(list(palette = palette))

  })
}
