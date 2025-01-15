
# ui --------------------------------------------------------------------------
descriptive_stats_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Descriptive Statistics', class = 'mini-header'),
    layout_sidebar(bg = '#02517d',
      sidebar = sidebar(
        bg = '#e3e3e4',
        uiOutput(ns('parameters')),
        checkboxGroupInput(
          ns('xg_central_tendency'),
          h6('Central Tendency'), inline = T,
          c('Mean' = 'mean', 'Median' = 'median')
        ),
        checkboxGroupInput(
          ns('xg_dispersion'), h6('Dispersion'), inline = T,
          c('Minimum' = 'min', 'Maximum' = 'max', 'IQR',
            'Range' = 'range', 'Variance' = 'var',
            'Standard Deviation' = 'sd')
        ),
        btn_task(ns('btn_stats'), 'Generate Table', icon('gear'))
      ),
      navset_card_pill(
        nav_panel(
          'Stats',
          card(full_screen = T,
            card_body(
              gt_output(ns('gt_stats')))
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
descriptive_stats_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    var_analysis <- reactive({
      df() |> names()
    })

    output$parameters <- renderUI({
      tagList(
        selectInput(ns('sel_var'), 'Variables', var_analysis(), multiple = T),
      )
    })

    df_stats <- reactive({
      subset(df(), select = input$sel_var)
    })

    desc_stats <- reactive({
      req(input$sel_var)
      desc_stats <- list()

      # central tendency
      if('mean' %in% input$xg_central_tendency){
        desc_stats$Mean <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) mean(x, na.rm = T) else NA})
      }

      if('median' %in% input$xg_central_tendency){
        desc_stats$Median <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) median(x, na.rm = T) else NA})
      }

      # dispersion
      if('min' %in% input$xg_dispersion){
        desc_stats$Min <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) mina(x) else NA})
      }

      if('max' %in% input$xg_dispersion){
        desc_stats$Max <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) mana(x) else NA})
      }

      if('IQR' %in% input$xg_dispersion){
        desc_stats$IQR <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) IQR(x, na.rm = T) else NA})
      }

      if('range' %in% input$xg_dispersion){
        desc_stats$Range <- sapply(
          df_stats(),
          \(x) {
            if(x |> is.numeric()){
              paste('[', range(x), ']', collapse = '--->')
            } else { NA }
            }
          )
      }

      if('var' %in% input$xg_dispersion){
        desc_stats$Variance <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) var(x, na.rm = T) else NA})
      }

      if('sd' %in% input$xg_dispersion){
        desc_stats[['Standard Deviation']] <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) sd(x, na.rm = T) else NA})
      }

      desc_stats
    })

    output$gt_stats <- render_gt(
      data.frame(
        Measures = names(desc_stats()),
        do.call(rbind, desc_stats())
      ) |>
        gt() |>
        opt_interactive()
    ) |> bindEvent(input$btn_stats)
  })
}
