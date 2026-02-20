
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
      req(var(), var2(), input$table_var, input$table_type)

      if(input$table_var == '1v') {

        validate(need(is.character(var()) || is.factor(var()) || is.logical(var()),
                      'Var must be character, factor or logical'))

        if(input$table_type == 'abs_table'){
          tab1 <- var() |> table()
        } else if(input$table_type == 'perc_table'){
          tab1 <- var() |> table() |> prop.table() * 100
        }

        tab1 |> as.data.frame()

      } else if (input$table_var == '2v'){
        req(var())
        req(var2())

        validate(need(
          var_name() != var2_name() &
            (is.character(var()) || is.factor(var()) || is.logical(var())) &
            (is.character(var2()) || is.factor(var2()) || is.logical(var2())),
          'Select two diferent variables of type character, factor or logical'))

        if(input$table_type == 'abs_table'){
          tab1 <- table(var(), var2())
        } else if(input$table_type == 'perc_table'){
          tab1 <- table(var(), var2()) |> prop.table() * 100
        }

        tab1 <- tab1 |> as.data.frame.matrix()

        cbind(var1 = rownames(tab1), tab1)
      }
    })

    table_values_gt <- reactive({
      req(table_values())
      req(input$table_var, input$table_type, var_name(), var2_name())

      if(input$table_var == '1v') {

        if(input$table_type == 'abs_table'){
          y_label <- 'Frequency'
        } else if(input$table_type == 'perc_table'){
          y_label <- 'Relative Frequency (%)'
        }

        table_values() |>
          gt() |>
          cols_label(
            Var1 = var_name(),
            Freq = y_label
          )
      } else if (input$table_var == '2v'){

        if(input$table_type == 'abs_table'){
          y_label <- var2_name()
        } else if(input$table_type == 'perc_table'){
          y_label <- paste(var2_name(), '(%)')
        }

        table_values() |>
          gt() |>
          cols_label(var1 = "") |>
          tab_spanner(
            label = var_name(),
            columns = var1
          ) |>
          tab_spanner(
            label = y_label,
            columns = names(table_values())[2:length(names(table_values()))]
          )
      }
    })

    output$table <- render_gt({
      req(table_values_gt())
      table_values_gt() |>
        opt_interactive()
    })

    # insert table of values to output ----------------------------------------
    insert_output_server('insert_table_values', table_values_gt, 'Table')

  })
}
