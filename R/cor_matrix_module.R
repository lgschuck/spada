
# ui --------------------------------------------------------------------------
cor_matrix_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = T,
    card_header('Correlation Matrix', class = 'mini-header'),
    layout_sidebar(
      class = 'card-sidebar',
      sidebar = sidebar(
        uiOutput(ns('parameters')),
        radioButtons(ns('radio_method'), 'Method',
                     c('Pearson' = 'pearson',
                       'Kendall' = 'kendall',
                       'Spearman' = 'spearman')),
        btn_task(ns('btn_cor_matrix'), 'Run', icon('gear'))
      ),
      navset_card_pill(
        nav_panel(
          'Correlation Matrix',
          card(
            card_body(plotOutput(ns('cor_plot'))),
            card_footer(insert_output_ui(ns('insert_cor_matrix')))
          )
        )
      )
    )
  )
}

# server ----------------------------------------------------------------------
cor_matrix_server <- function(id) {
  moduleServer(id, function(input, output, session) {
	  ns <- session$ns

	  df <- reactive(get_act_dt(session))

    # outupt objects ----------------------------------------------------------
    df_active <- reactive({
      req(df())
      df()[, lapply(df(), is.numeric) == T, with = F]
    })

    var_analysis <- reactive({
      df_names <- df_active() |> names()

      var_analysis <- session$userData$dt$act_meta()[perc_nas != 1, var]

      df_names[df_names %in% var_analysis]
    })

    output$parameters <- renderUI({
      tagList(
        selectizeInput(
          ns('sel_vars'),
          'Variables',
          var_analysis(),
          var_analysis(),
          multiple = T,
          options = list(plugins = list('remove_button', 'clear_button'))
        ),
        p('* Showing only numeric variables')
      )
    })

    # run cor matrix ----------------------------------------------------------
    df_cor <- reactive({
      req(input$sel_vars, input$radio_method)

      df_cor <- cor(subset(df(), select = input$sel_vars),
                    method = input$radio_method,
                    use = 'na.or.complete') |>
        as.data.frame()

      df_cor <- cbind(Var1 = rownames(df_cor), df_cor) |> as.data.table()

      df_cor <- melt(
        data = df_cor,
        id.vars = 'Var1',
        variable.name = 'Var2',
        value.name = 'value'
      )

      df_cor[, label := round(value, digits = 3)]
      df_cor[, method := input$radio_method]
      df_cor

    }) |> bindEvent(input$btn_cor_matrix)

    cor_matrix <- reactive({
      req(df_cor())

      method <- df_cor()$method[1]

      text_size <- max(
        6,
        10 / sqrt(length(df_cor()$Var1))
      )

      ggplot(
        data = df_cor(),
        mapping = aes(x = Var1, y = Var2, fill = value)
      ) + geom_tile(color = 'white') +
        labs(title = paste('Correlation (', method, ')'),
             x = NULL, y = NULL, fill = '') +
        theme(
          panel.grid = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(color = session$userData$conf$plot_title_color,
                                    size = 16, face = 'bold')
        ) +
        geom_text(aes(label = label), size = text_size, color = '#000000') +
        scale_fill_gradient2(
          low = danger,
          mid = '#ffffff',
          high = secondary,
          midpoint = 0,
          limits = c(-1, 1)
        )
    })

    output$cor_plot <- renderPlot({
      cor_matrix()
    })

    # insert cor matrix to output ---------------------------------------------
    insert_output_server(
      'insert_cor_matrix',
      reactive(plot_tag(cor_matrix())),
      'Correlation Matrix'
    )

  })
}
