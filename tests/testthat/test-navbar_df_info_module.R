# tests/testthat/test-navbar_df_info_module.R

# test df info UI -------------------------------------------------------------
test_that("Test render df_info UI", {
  testServer(navbar_df_info_server, args = list(app_session = NULL), {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris |>  as.data.table()),
      act_name = 'iris'
    )

    session$userData$dt_names <- reactive({
      req(session$userData$dt$dt)
      names(session$userData$dt$dt)
    })

    session$userData$dt$df_info <- reactive({
      req(session$userData$dt)

      lapply(session$userData$dt$dt, df_info)
    })

    session$userData$dt$act_meta <- reactive({
      req(session$userData$dt$df_info())
      session$userData$dt$df_info()[[session$userData$dt$act_name]]
    })

    session$userData$dt$act_row_col <- reactive({
      req(session$userData$dt$df_info())
      paste(session$userData$dt$act_meta() |> pull(rows) |> head(1) |> f_num(dig = 1),
            '/', session$userData$dt$act_meta() |> pull(cols) |> head(1) |> f_num())
    })

    session$userData$dt$act_col_nas <- reactive({
      req(session$userData$dt$df_info())
      session$userData$dt$act_meta() |>
        filter(n_nas > 0) |>
        nrow()
    })

    session$userData$dt$act_size <- reactive({
      req(session$userData$dt$df_info())
      (object.size(get_act_dt(session)) / 2^20) |>
        as.numeric() |> round(2)
    })

    expect_true(is.list(output$navbar_df_info))

    expect_true(grepl('iris', output$navbar_df_info$html))
    expect_true(grepl('Size', output$navbar_df_info$html))
    expect_true(grepl('0', output$navbar_df_info$html))
    expect_true(grepl('Rows/Columns:\n  150.0 / 5\n', output$navbar_df_info$html))
    expect_true(grepl("Columns with NA's:\n  0", output$navbar_df_info$html))

  })
})
