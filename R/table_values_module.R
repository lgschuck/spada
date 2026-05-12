
# ui --------------------------------------------------------------------------
table_values_ui <- function(id) {
  ns <- NS(id)

  tagList(
    card_body(
      fluidRow(
        column(4,
               radioGroupButtons(
                 ns('table_var'),
                 'Table variables:',
                 c('1 Variable' = '1v', '2 Variables' = '2v'),
                 size = 'sm',
                 individual = T
               )
        ),
        column(4,
               radioGroupButtons(
                 ns('table_type'),
                 'Table type:',
                 c('Absolute Values' = 'abs_table', 'Percent Values' = 'perc_table'),
                 size = 'sm',
                 individual = T
               )
        ),
      ),
      gt_output(ns('table')),
    ),
    card_footer(insert_output_ui(ns('insert_table_values')))
  )
}

# server ----------------------------------------------------------------------
table_values_server <- function(id, var, var2, var_name, var2_name) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    table_values <- reactive({
      req(var(), input$table_var, input$table_type)

      var <- var()

      if(input$table_var == '1v') {

        validate(need(is.character(var) || is.factor(var) || is.logical(var),
                      'Var must be character, factor or logical'))

        tab <- var |> qtab()
        if(input$table_type == 'perc_table') tab <- tab |> prop.table()

        tab <- tab |> as.data.frame()
        names(tab) <- c('Var1', 'Freq')
        tab

      } else if (input$table_var == '2v'){
        req(var2())
        var2 <- var2()

        validate(need(
          var_name() != var2_name() && is_categorical(var) && is_categorical(var2),
          'Select two different variables of type character, factor or logical')
        )

        tab <- qtab(var, var2)

        if(input$table_type == 'perc_table') tab <- tab |> prop.table()

        tab <- tab |> as.data.frame.matrix()

        cbind(Var1 = rownames(tab), tab)
      }
    })

    table_values_gt <- reactive({
      req(table_values())
      req(input$table_var, input$table_type, var_name())

      var_name <- var_name()
      tab <- table_values()

      if(input$table_var == '1v') {

        y_label <- if(input$table_type == 'abs_table') 'Frequency' else 'Relative Frequency'

        tab <- tab |>
          gt() |>
          cols_label(
            Var1 = var_name,
            Freq = y_label
          )

        if(input$table_type == 'abs_table') {
          tab |> fmt_number(columns = Freq)
        } else {
          tab |> fmt_percent(columns = Freq, decimals = 4)
        }

      } else if(input$table_var == '2v'){
        req(var2_name())
        var2_name <- var2_name()

        cols <- names(tab)[-1]

        tab <- tab |>
          gt() |>
          cols_label(Var1 = '') |>
          tab_spanner(
            label = var_name,
            columns = Var1
          ) |>
          tab_spanner(
            label = var2_name,
            columns = cols
          )

        if(input$table_type == 'abs_table') {
          tab |> fmt_number()
        } else {
          tab |> fmt_percent(decimals = 4)
        }
      }
    })

    output$table <- render_gt({
      req(table_values_gt())
      table_values_gt() |>
        opt_interactive(
          page_size_default = 10,
          use_filters = T,
          use_resizers = T,
          use_highlight = T,
          use_compact_mode = T,
          use_text_wrapping = F,
          use_page_size_select = T
        )
    })

    # insert table of values to output ----------------------------------------
    insert_output_server('insert_table_values', table_values_gt, 'Table')

  })
}
