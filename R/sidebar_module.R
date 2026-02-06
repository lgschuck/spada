
# ui --------------------------------------------------------------------------
sidebar_ui <- function(id) {
  ns <- NS(id)

  sidebar(
    open = T,
    accordion(
      open = T,
      accordion_panel(
        class = 'accordion-sidebar',
        'Active Dataset',
        icon = bs_icon('check2-square', size = '1.75em'),
        selectInput(ns('sel_act_dt'), NULL, choices = NULL, selected = NULL),
        uiOutput(ns('df_info')),
        fluidRow(
          column(3, actionButton(ns('df_btn_overview'), '',
                                 icon('magnifying-glass'), class = 'mini-btn') |>
                   tooltip('Overview', placement = 'bottom')),
          column(3, actionButton(ns('df_btn_meta'), '', bs_icon('info-circle'),
                                 class = 'mini-btn') |>
                   tooltip('Metadata', placement = 'bottom')),
          column(3, actionButton(ns('df_btn_change'), '', icon('shuffle'),
                                 class = 'mini-btn') |>
                   tooltip('Change dataset', placement = 'bottom')),
          column(3, actionButton(ns('df_btn_explore'), '', icon('chart-simple'),
                                 class = 'mini-btn') |>
                   tooltip('Exploratory Analysis', placement = 'bottom')),
        )
      )
    ),
    accordion(
      open = T,
      accordion_panel(
        class = 'accordion-sidebar',
        'Datasets',
        icon = bs_icon('stack', size = '1.75em'),
        selectInput(ns('sel_datasets_names'), '', choices = NULL),
        actionButton(ns('btn_preview_dt'), 'Preview', icon('magnifying-glass'),
                     class = 'btn-task') |>
          popover(htmlOutput(ns('df_preview')),
                  options = list(customClass = 'preview-dt-popup'))
      )
    ),
    btn_task(ns('btn_save_session'), 'Save Session', icon('save'))
  )
}

# server ----------------------------------------------------------------------
sidebar_server <- function(id, app_session) {
  moduleServer(id, function(input, output, session) {
  	ns <- session$ns

  	# active dataset info -----------------------------------------------------
    output$df_info <- renderUI({
      div(
        style = 'font-size: 0.9rem;',
        p(
          'Rows/Columns: ',
          textOutput(ns('row_col'), inline = TRUE)
        ),
        p(
          "Columns with NA's: ",
          textOutput(ns('col_nas'), inline = TRUE)
        ),
        p(
          'Size (MB): ',
          textOutput(ns('size_mb'), inline = TRUE)
        )
      )
    })

    output$row_col <- renderText({
      session$userData$dt$act_mini_meta()[['row_col']]
    })

    output$col_nas <- renderText({
      session$userData$dt$act_mini_meta()[['col_nas']]
    })

    output$size_mb <- renderText({
      session$userData$dt$act_mini_meta()[['size']]
    })

    # update active dataset list --------
    observe({
      req(session$userData$dt$dt, session$userData$dt$act_name)

      updateSelectInput(
        session,
        'sel_act_dt',
        choices = c(
          session$userData$dt$act_name,
          setdiff(session$userData$dt_names(), session$userData$dt$act_name)
        ),
        selected = session$userData$dt$act_name
      )

    })

    # update active dataset --------
    observe({
      req(input$sel_act_dt)
      session$userData$dt$act_name <- input$sel_act_dt
      session$userData$dt$bkp0 <- copy(get_act_dt(session))
      session$userData$dt$bkp <- NULL

    }) |> bindEvent(input$sel_act_dt, ignoreInit = T)

    # mini buttons --------
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

    # list of datasets --------------------------------------------------------
    observe({
      req(session$userData$dt$dt, session$userData$dt$act_name)
      choices <- c(
        session$userData$dt$act_name,
        setdiff(session$userData$dt_names(), session$userData$dt$act_name)
      )

      updateSelectInput(session, 'sel_datasets_names', choices = choices)
    })

    # preview dataset -------------------------
    output$df_preview <- renderUI({
      req(session$userData$dt$dt, input$sel_datasets_names)

      n <- min(5, session$userData$dt$dt[[input$sel_datasets_names]] |> nrow())
      df <- session$userData$dt$dt[[input$sel_datasets_names]][1:n, ]

      gt::gt(df) |>
        tab_header(input$sel_datasets_names) |>
        tab_options(heading.align = 'left') |>
        gt::as_raw_html() |>
        HTML()
    })

    # save session ------------------------------------------------------------
    observe({
      save_session(session$userData$conf$data_dir,
                   session$userData$out$elements,
                   session$userData$dt$dt)

      save_conf(session$userData$conf$conf_dir,
                reactiveValuesToList(session$userData$conf))

      show_toast(
        title = 'Session saved',
        type = 'info',
        position = 'bottom-start',
        timer = 2000,
        timerProgressBar = F,
        width = '250px'
      )

    }) |> bindEvent(input$btn_save_session)
  })
}
