
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

    dt <- reactive({
      req(session$userData$dt$dt)
      get_act_dt(session)
    })

    dt_meta <- reactive({
      req(session$userData$dt$act_meta())
      copy(session$userData$dt$act_meta())
    })

    output$var_num_vars <- renderText(sapply(dt(), is.numeric) |> sum())
    output$var_char_vars <- renderText(sapply(dt(), is.character) |> sum())
    output$var_factor_vars <- renderText(sapply(dt(), is.factor) |> sum())
    output$var_date_vars <- renderText(sapply(dt(), is_date) |> sum())

    output$n_rows <- renderText( dt_meta()[1, ]$rows |> f_num() )

    output$var_most_valid <- renderText(
      if (dt_meta()[n_valid > 0, ] |> nrow() < 1) {
        '---'
      } else {
        setorderv(dt_meta(), 'n_valid', -1, na.last = T)[1, var]
      }
    )

    output$var_most_valid_n_valid <- renderText(
      if (dt_meta()[n_valid > 0, ] |> nrow() < 1) {
        '0'
      } else {
        setorderv(dt_meta(), 'n_valid', -1, na.last = T)[1, f_num(n_valid)]
      }
    )

    output$var_most_unique <- renderText(
      if (dt_meta()[n_unique > 0, ] |> nrow() < 1) {
        '---'
      } else {
        setorderv(dt_meta(), 'n_unique', -1, na.last = T)[1, var]
      }
    )

    output$var_most_unique_n_unique <- renderText(
      if (dt_meta()[n_unique > 0, ] |> nrow() < 1) {
        '0'
      } else {
        setorderv(dt_meta(), 'n_unique', -1, na.last = T)[1, f_num(n_unique)]
      }
    )

    output$var_most_zeros <- renderText(
      if (dt_meta()[n_zero > 0, ] |> nrow() < 1) {
        '---'
      } else {
        setorderv(dt_meta(), 'n_zero', -1, na.last = T)[1, var]
      }
    )

    output$var_most_zeros_n_zeros <- renderText(
      if (dt_meta()[n_zero > 0, ] |> nrow() < 1) {
        '0'
      } else {
        setorderv(dt_meta(), 'n_zero', -1, na.last = T)[1, f_num(n_zero)]
      }
    )

    output$var_most_nas <- renderText({
      if (dt_meta()[n_nas > 0, ] |> nrow() < 1) {
        '---'
      } else {
        setorderv(dt_meta()[n_nas > 0, ],
                  c('n_nas', 'perc_nas'), c(-1, -1), na.last = T)[1, var]
      }
    })

    output$var_most_nas_n <- renderText({
      if (dt_meta()[n_nas > 0, ] |> nrow() < 1) {
        '0'
      } else {
        setorderv(dt_meta()[n_nas > 0, ],
                  c('n_nas', 'perc_nas'), c(-1, -1), na.last = T)[1, f_num(n_nas)]
      }
    })

    output$var_max_value <- renderText(
      if (dt_meta()[!is.na(max), ] |> nrow() < 1) {
        '---'
      } else {
        setorderv(dt_meta(), 'max', -1, na.last = T)[1, var]
      }
    )

    output$max_value <- renderText(
      if (dt_meta()[!is.na(max), ] |> nrow() < 1) {
        '0'
      } else {
        setorderv(dt_meta(), 'max', -1, na.last = T)[1, f_num(max, dig = 3)]
      }
    )

    output$var_min_value <- renderText(
      if (dt_meta()[!is.na(min), ] |> nrow() < 1) {
        '---'
      } else {
        dt_meta() |> arrange(min) |> head(1) |> pull(var)
        setorderv(dt_meta(), 'min', na.last = T)[1, var]
      }
    )

    output$min_value <- renderText(
      if (dt_meta()[!is.na(min), ] |> nrow() < 1) {
        '0'
      } else {
        setorderv(dt_meta(), 'min', na.last = T)[1, f_num(min, dig = 3)]
      }
    )

    output$var_biggest_size <- renderText(
      setorderv(dt_meta(), 'size', -1, na.last = T)[1, var]
    )

    output$var_biggest_size_size <- renderText(
      setorderv(dt_meta(), 'size', -1, na.last = T)[1, f_num(size)]
    )

  })
}
