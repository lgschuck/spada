
# ui --------------------------------------------------------------------------
data_highlights_ui <- function(id) {
  ns <- NS(id)

  card_body(
    layout_column_wrap(
      value_box(
        title = 'Numeric Vars',
        value = textOutput(ns('var_num_vars')),
        showcase = bs_icon('123'),
        theme = 'bg-gradient-yellow-orange'
      ),
      value_box(
        title = 'Character Vars',
        value = textOutput(ns('var_char_vars')),
        showcase = bs_icon('alphabet'),
        theme = 'bg-gradient-blue-indigo'
      ),
      value_box(
        title = 'Factor Vars',
        value = textOutput(ns('var_factor_vars')),
        showcase = bs_icon('diagram-3'),
        theme = 'bg-gradient-green-blue'
      ),
      value_box(
        title = 'Date Vars',
        value = textOutput(ns('var_date_vars')),
        showcase = bs_icon('calendar3'),
        theme = 'bg-gradient-purple-indigo'
      )
    ),
    layout_column_wrap(
      value_box(
        title = 'Rows',
        value = textOutput(ns('n_rows')),
        showcase = bs_icon('list'),
        theme = 'bg-gradient-blue-purple'
      ),
      value_box(
        title = 'Most valid values',
        value = textOutput(ns('var_most_valid')),
        showcase = bs_icon('list-check'),
        theme = 'bg-gradient-indigo-yellow',
        p('Number of valid:', textOutput(ns('var_most_valid_n_valid'), inline = T))
      ) |> tooltip('Showing 1, there may be ties', placement = 'top'),
      value_box(
        title = 'Most unique values',
        value = textOutput(ns('var_most_unique')),
        showcase = bs_icon('fingerprint'),
        theme = 'bg-gradient-indigo-green',
        p('Number of unique:', textOutput(ns('var_most_unique_n_unique'), inline = T))
      ) |> tooltip('Showing 1, there may be ties', placement = 'top'),
      value_box(
        title = 'Most zeros',
        value = textOutput(ns('var_most_zeros')),
        showcase = bs_icon('0-circle'),
        theme = 'bg-gradient-orange-indigo',
        p('Number of zeros:', textOutput(ns('var_most_zeros_n_zeros'), inline = T))
      ) |> tooltip('Showing 1, there may be ties', placement = 'top')
    ),
    layout_column_wrap(
      value_box(
        title = "Most NA's",
        value = textOutput(ns('var_most_nas')),
        showcase = bs_icon('database-x'),
        theme = 'bg-gradient-red-indigo',
        p("Number of NA's:", textOutput(ns('var_most_nas_n'), inline = T), ' rows')
      ) |> tooltip('Showing 1, there may be ties', placement = 'top'),
      value_box(
        title = 'Max value',
        value = textOutput(ns('var_max_value'), inline = T),
        showcase = bs_icon('graph-up-arrow', placement = 'top'),
        theme = 'bg-gradient-blue-green',
        p('Max value:', textOutput(ns('max_value'), inline = T))
      ) |> tooltip('Showing 1, there may be ties', placement = 'top'),
      value_box(
        title = 'Min value',
        value = textOutput(ns('var_min_value')),
        showcase = bs_icon('graph-down-arrow'),
        theme = 'bg-gradient-pink-indigo',
        p('Min value:', textOutput(ns('min_value'), inline = T))
      ) |> tooltip('Showing 1, there may be ties', placement = 'top'),
      value_box(
        title = 'Biggest size',
        value = textOutput(ns('var_biggest_size')),
        showcase = bs_icon('sd-card'),
        theme = 'bg-gradient-teal-indigo',
        p('Size:', textOutput(ns('var_biggest_size_size'), inline = T), 'Bytes')
      ) |> tooltip('Showing 1, there may be ties', placement = 'top')
    )
  )

}

# server ----------------------------------------------------------------------
data_highlights_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    df <- reactive(session$userData$df$act)
    df_metadata <- reactive(session$userData$df$act_meta())

    output$var_num_vars <- renderText(sapply(df(), is.numeric) |> sum())
    output$var_char_vars <- renderText(sapply(df(), is.character) |> sum())
    output$var_factor_vars <- renderText(sapply(df(), is.factor) |> sum())
    output$var_date_vars <- renderText(sapply(df(), is_date) |> sum())

    output$n_rows <- renderText(
      df() |> nrow() |> f_num()
    )
    output$var_most_valid <- renderText(
      if(df_metadata() |> filter(n_valid > 0) |> nrow() < 1) { '---'
      } else {
        df_metadata() |> arrange(-n_valid) |> head(1) |> pull(var)
      }
    )

    output$var_most_valid_n_valid <- renderText(
      if(df_metadata() |> filter(n_valid > 0) |> nrow() < 1) { '0'
      } else {
        df_metadata() |> arrange(-n_valid) |> head(1) |> pull(n_valid) |> f_num()
      }
    )

    output$var_most_unique <- renderText(
      if(df_metadata() |> filter(n_unique > 0) |> nrow() < 1) { '---'
      } else {
        df_metadata() |> arrange(-n_unique) |> head(1) |> pull(var)
      }
    )

    output$var_most_unique_n_unique <- renderText(
      if(df_metadata() |> filter(n_unique > 0) |> nrow() < 1) { '0'
      } else {
        df_metadata() |> arrange(-n_unique) |> head(1) |> pull(n_unique) |> f_num()
      }
    )

    output$var_most_zeros <- renderText(
      if(df_metadata() |> filter(n_zero > 0) |> nrow() < 1) { '---'
      } else {
        df_metadata() |> arrange(-n_zero) |> head(1) |> pull(var)
      }
    )

    output$var_most_zeros_n_zeros <- renderText(
      if(df_metadata() |> filter(n_zero > 0) |> nrow() < 1) { '0'
      } else {
        df_metadata() |> arrange(-n_zero) |> head(1) |> pull(n_zero) |> f_num()
      }
    )

    output$var_most_nas <- renderText(
      {
        if(df_metadata() |> filter(n_nas > 0) |> nrow() < 1) { '---'
        } else {
          df_metadata() |> filter(n_nas > 0)|> arrange(-n_nas, -perc_nas) |>
            head(1) |> pull(var) }
      }
    )

    output$var_most_nas_n <- renderText(
      {
        if(df_metadata() |> filter(n_nas > 0) |> nrow() < 1) { '0'
        } else {
          df_metadata() |> filter(n_nas > 0)|> arrange(-n_nas, -perc_nas) |>
            head(1) |> pull(n_nas) |> f_num()}
      }
    )

    output$var_max_value <- renderText(
      if(df_metadata() |> filter(!is.na(max)) |> nrow() < 1) { '---'
      } else {
        df_metadata() |> arrange(-max) |> head(1) |> pull(var)
      }
    )

    output$max_value <- renderText(
      if(df_metadata() |> filter(!is.na(max)) |> nrow() < 1) { '0'
      } else {
        df_metadata() |> arrange(-max) |> head(1) |> pull(max) |> f_num(dig = 3)
      }
    )

    output$var_min_value <- renderText(
      if(df_metadata() |> filter(!is.na(min)) |> nrow() < 1) { '---'
      } else {
        df_metadata() |> arrange(min) |> head(1) |> pull(var)
      }
    )

    output$min_value <- renderText(
      if(df_metadata() |> filter(!is.na(min)) |> nrow() < 1) { '0'
      } else {
        df_metadata() |> arrange(min) |> head(1) |> pull(min) |> f_num(dig = 3)
      }
    )

    output$var_biggest_size <- renderText(
      df_metadata() |> arrange(-size) |> head(1) |> pull(var)
    )

    output$var_biggest_size_size <- renderText(
      df_metadata() |> arrange(-size) |> head(1) |> pull(size) |> f_num()
    )

  })
}
