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

    session$userData$dt$act_mini_meta <- reactive({
      req(session$userData$dt$act_meta())

      list(
        'row_col' = paste(
          f_int(session$userData$dt$act_meta()[['rows']][1]),
          '/',
          f_int(session$userData$dt$act_meta()[['cols']][1])
        ),
        'col_nas' = session$userData$dt$act_meta()[n_nas > 0, ] |> nrow()
      )
    })

    expect_true(is.list(output$navbar_df_info))
    expect_true(grepl('0', output$navbar_df_info$html))
    expect_true(grepl('Rows/Columns:\n  150 / 5\n', output$navbar_df_info$html))
    expect_true(grepl("Columns with NA's:\n  0", output$navbar_df_info$html))
  })
})
