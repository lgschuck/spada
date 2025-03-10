
# ui --------------------------------------------------------------------------
descriptive_stats_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Descriptive Statistics', class = 'mini-header'),
    layout_sidebar(
      class = 'card-sidebar',
      sidebar = sidebar(
        uiOutput(ns('parameters')),
        checkboxGroupInput(
          ns('xg_central_tendency'),
          h6('Central Tendency'), inline = T,
          c('Mean' = 'mean', 'Geometric Mean' = 'gmean', 'Harmonic Mean' = 'hmean',
            'Median' = 'median', 'Mode' = 'mode'),
          c('mean', 'gmean', 'hmean', 'median', 'mode')
        ),
        checkboxGroupInput(
          ns('xg_dispersion'), h6('Dispersion'), inline = T,
          c('Minimum' = 'min', 'Maximum' = 'max', 'IQR',
            'Range' = 'range', 'Variance' = 'var',
            'Standard Deviation' = 'sd'),
          c('min', 'max', 'IQR', 'range', 'var', 'sd')
        ),
        checkboxGroupInput(
          ns('xg_shape'), h6('Shape'), inline = T,
          c('Skewness' = 'skew', 'Kurtosis' = 'kurt'), c('skew', 'kurt')
        ),
        numericInput(ns('table_digits'), 'Digits', 2, 0, 9, 1),
        btn_task(ns('btn_stats'), 'Generate Table', icon('gear'))
      ),
      navset_card_pill(
        nav_panel(
          'Stats',
          card(full_screen = T,
            card_body(
              gt_output(ns('gt_stats')),
              uiOutput(ns('conditional_save_gt'))
            )
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
descriptive_stats_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

    var_analysis <- reactive({
      df() |> names()
    })

    output$parameters <- renderUI({
      tagList(
        selectizeInput(
          ns('sel_var'), 'Variables', var_analysis(), var_analysis()[1],
          multiple = T,
          options = list(plugins = list('remove_button', 'clear_button'))
        ),
      )
    })

    df_stats <- reactive({
      req(input$sel_var)
      subset(df(), select = input$sel_var)
    })


    # calculate stats ---------------------------------------------------------
    desc_stats <- reactive({
      req(input$sel_var)
      desc_stats <- list()

      fmt_digits <- min(max(0, input$table_digits), 9)

      # central tendency
      if('mean' %in% input$xg_central_tendency){
        desc_stats$Mean <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) mean(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
      }

      if('gmean' %in% input$xg_central_tendency){
        desc_stats$Gmean <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) Gmean(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
      }

      if('hmean' %in% input$xg_central_tendency){
        desc_stats$Hmean <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) Hmean(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
      }

      if('median' %in% input$xg_central_tendency){
        desc_stats$Median <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) median(x, na.rm = T) |> f_num(dig = fmt_digits)  else NA })
      }

      if('mode' %in% input$xg_central_tendency){
        desc_stats$Mode <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric() ||
                   x |> is.character() ||
                   x |> is.factor()){
            x_mode <- Mode(x, na.rm = T) |> f_num(dig = fmt_digits)
            if(is.na(x_mode) |> all()) NA else paste(x_mode, collapse = ' | ')
          } else { NA }
        })
      }

      # dispersion
      if('min' %in% input$xg_dispersion){
        desc_stats$Min <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) mina(x) |> f_num(dig = fmt_digits) else NA })
      }

      if('max' %in% input$xg_dispersion){
        desc_stats$Max <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) mana(x) |> f_num(dig = fmt_digits) else NA })
      }

      if('IQR' %in% input$xg_dispersion){
        desc_stats$IQR <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) IQR(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
      }

      if('range' %in% input$xg_dispersion){
        desc_stats$Range <- sapply(
          df_stats(),
          \(x) {
            if(x |> is.numeric()){
              paste('[', range(x) |> f_num(dig = fmt_digits) , ']', collapse = '--->')
            } else { NA }
            }
          )
      }

      if('var' %in% input$xg_dispersion){
        desc_stats$Variance <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) var(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
      }

      if('sd' %in% input$xg_dispersion){
        desc_stats[['Standard Deviation']] <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) sd(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
      }

      if('skew' %in% input$xg_shape){
        desc_stats[['Skewness']] <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) Skew(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
      }

      if('kurt' %in% input$xg_shape){
        desc_stats[['Kurtosis']] <- sapply(
          df_stats(),
          \(x) {if(x |> is.numeric()) Kurt(x, na.rm = T) |> f_num(dig = fmt_digits) else NA })
      }

      desc_stats
    })

    # gt table ----------------------------------------------------------------
    gt_stats <- reactive({
      data.frame(
        Measures = names(desc_stats()),
        do.call(rbind, desc_stats())
      ) |>
        gt()
    }) |> bindEvent(input$btn_stats)

    output$gt_stats <- render_gt({
      req(gt_stats())
      gt_stats() |>
        cols_align(align = 'right') |>
        cols_align('left', Measures) |>
        sub_missing(missing_text = '-') |>
        sub_values(values = 'NA', replacement = '-') |>
        sub_values(values = 'Gmean', replacement = 'Geometric Mean') |>
        sub_values(values = 'Hmean', replacement = 'Harmonic Mean') |>
        opt_interactive(page_size_default = 25)
    })

    # save table module -------------------------------------------------------
    save_gt_server('pA_desciptive_stats_save_gt', gt_stats)

    output$conditional_save_gt <- renderUI({
      req(gt_stats())
      save_gt_ui(ns('pA_desciptive_stats_save_gt'))
    })

  })
}
