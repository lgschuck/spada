
# ui --------------------------------------------------------------------------
config_ui <- function(id) {
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
              uiOutput(ns('ui_fill_color')),
              uiOutput(ns('ui_line_color')),
              uiOutput(ns('ui_title_color')),
              btn_task(ns('btn_reset_colors'), 'Reset', icon('rotate'),
                       style = 'margin-top: 27px !important;')
            ),
            plotOutput(ns('sample_plot'))
          )
        ),
        card(
          card_body(
            fluidRow(accordion(
              accordion_panel(
                'Theme',
                layout_columns(
                  col_widths = c(7, 5),
                  selectInput(
                    ns('theme_choice'),
                    NULL,
                    choices = c('Spada' = 'spada_theme', 'Dark Spada' = 'spada_dark_theme')
                  ),
                  actionButton(ns('btn_theme'), 'Apply', icon('check'), class = 'btn-task')
                ),
                icon = bs_icon('palette')
              ),
              open = F
            )),
            fluidRow(accordion(
              accordion_panel(
                'Size of input files (MB)',
                layout_columns(
                  col_widths = c(7, 5),
                  numericInput(
                    ns('input_file_size'),
                    NULL,
                    value = 1000,
                    min = 0,
                    step = 500
                  ),
                  actionButton(ns('btn_file_size'), 'Apply', icon('check'), class = 'btn-task')
                ),
                icon = bs_icon('upload')
              ),
              open = F
            )),
            fluidRow(accordion(
              accordion_panel(
                'Config Directories',
                h5('Config'),
                textOutput(ns('conf_dir')),
                h5('Data'),
                textOutput(ns('conf_data_dir')),
                icon = bs_icon('folder-check')
              ),
              open = F
            )),
            fluidRow(accordion(
              accordion_panel(
                'Session',
                radioGroupButtons(
                  ns('radio_restore_session'), 'Restore Session data at startup',
                  c('Always' = 'always', 'Never' = 'never'),
                  size = 'sm', individual = T),
                radioGroupButtons(
                  ns('radio_save_session'), 'Save Session data on exit',
                  c('Always' = 'always', 'Ask' = 'ask', 'Never' = 'never'),
                  size = 'sm', individual = T),
                actionButton(ns('btn_save_session_conf'), 'Apply', icon('check'), class = 'btn-task'),
                icon = bs_icon('sliders')
              ),
              open = F
            )),
            fluidRow(accordion(
              accordion_panel(
                'Plot Limit (N rows)',
                layout_columns(
                  col_widths = c(7, 5),
                  numericInput(
                    ns('input_plot_limit'),
                    NULL,
                    value = 100000,
                    min = 0,
                    step = 50000
                  ),
                  actionButton(ns('btn_plot_limit'), 'Apply', icon('check'), class = 'btn-task')
                ),
                icon = bs_icon('graph-up')
              ),
              open = F
            )),
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
config_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  # render colorpickr -----------------------
	  output$ui_fill_color <- renderUI({
	    req(session$userData$conf$plot_fill_color)
	    colorPickr(
  	    inputId = ns('sel_fill'),
  	    label = 'Fill color',
  	    selected = session$userData$conf$plot_fill_color,
  	    update = 'save'
	    )
	  })

	  output$ui_line_color <- renderUI({
	    req(session$userData$conf$plot_line_color)
	    colorPickr(
	      inputId = ns('sel_line'),
	      label = 'Line color',
	      selected = session$userData$conf$plot_line_color,
	      update = 'save'
	    )
	  })

	  output$ui_title_color <- renderUI({
	    req(session$userData$conf$plot_title_color)
	    colorPickr(
	      inputId = ns('sel_title'),
	      label = 'Title color',
	      selected = session$userData$conf$plot_title_color,
	      update = 'save'
	    )
	  })

	  # palette ---------------------------------
	  palette <- reactive({
	    list('fill' = input$sel_fill, 'line' = input$sel_line,
	         'title' = input$sel_title)
	  })

	  observe({
	    req(input$sel_fill, input$sel_line)
	    session$userData$conf$plot_fill_color <- palette()[['fill']]
	    session$userData$conf$plot_line_color <- palette()[['line']]
	    session$userData$conf$plot_title_color <- palette()[['title']]
	  })

	  # reset colors ---------------------------
	  observe({
	    updateColorPickr(session = session,
	                     inputId = 'sel_fill',
	                     value = plot_fill_color)
	    updateColorPickr(session = session,
	                     inputId = 'sel_line',
	                     value = plot_line_color)
	    updateColorPickr(session = session,
	                     inputId = 'sel_title',
	                     value = plot_title_color)
	  }) |> bindEvent(input$btn_reset_colors)

	  # sample plot to show picked colors ------
	  plot_values <- rnorm(1e3)
	  output$sample_plot <- renderPlot({
	    req(palette())
	    hist(
	      plot_values,
	      col = palette()[['fill']],
	      xlab = '',
	      ylab = '',
	      main = 'Title',
	      col.main = palette()[['title']],
	      cex.main = 3
	    )
	    abline(h = 100, col = palette()[['line']], lwd = 3)

	  }) |> bindCache(palette())

	  # directories -----------------------------
	  output$conf_dir <- renderText({
	    req(session$userData$conf$conf_dir)
	    session$userData$conf$conf_dir
    })

	  output$conf_data_dir <- renderText({
	    req(session$userData$conf$data_dir)
	    session$userData$conf$data_dir
	   })

	  # set theme conf value --------------------
	  observe({
	    req(session$userData$conf$theme)
	    updateSelectInput(
	      session = session,
	      inputId = 'theme_choice',
	      selected = session$userData$conf$theme
	    )
	  })

	  # set file size conf value ----------------
	  observe({
	    req(session$userData$conf$file_size)
	    updateSelectInput(
	      session = session,
	      inputId = 'input_file_size',
	      selected = session$userData$conf$file_size
	    )
	  })

	  # set restore sesison conf value ----------
	  observe({
	    req(session$userData$conf$restore_session)
	    updateRadioGroupButtons(
	      session = session,
	      inputId = 'radio_restore_session',
	      selected = session$userData$conf$restore_session
	    )
	  })

	  # set save sesison conf value -------------
	  observe({
	    req(session$userData$conf$save_session)
	    updateRadioGroupButtons(
	      session = session,
	      inputId = 'radio_save_session',
	      selected = session$userData$conf$save_session
	    )
	  })

    # input file size ------------------------
    max_request_size <- reactive(input$input_file_size * 1024 ^ 2)

    observe({
      if(!isTruthy(input$input_file_size) || input$input_file_size < 1) {
        msg('Value must be > 1')
      } else {
        options(shiny.maxRequestSize = max_request_size())
        session$userData$conf$file_size <- input$input_file_size
        msg('New limit applied')
      }
    }) |> bindEvent(input$btn_file_size)

    # change theme ---------------------------
    observe({
      session$setCurrentTheme(

        if(input$theme_choice == 'spada_theme'){
          spada_theme
        } else if(input$theme_choice == 'spada_dark_theme'){
          spada_dark_theme
        }
      )
      session$userData$conf$theme <- input$theme_choice

      msg('New theme applied')
    }) |> bindEvent(input$btn_theme)

    # session conf ---------------------------
    observe({
      session$userData$conf$restore_session <- input$radio_restore_session
      session$userData$conf$save_session <- input$radio_save_session

      msg('New settings applied')
    }) |> bindEvent(input$btn_save_session_conf)

    # input plot limit ------------------------
    observe({
      if(!isTruthy(input$input_plot_limit) || input$input_plot_limit < 1) {
        msg('Value must be > 1')
      } else {
        session$userData$conf$plot_limit <- input$input_plot_limit
        msg('New limit applied')
      }
    }) |> bindEvent(input$btn_plot_limit)

  })
}
