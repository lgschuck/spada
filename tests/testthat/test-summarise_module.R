# tests/testthat/test-summarise_module.R

dt <- iris |> as.data.table()

# test summarise - distinct ---------------------------------------------------
test_that('Test summarise distinct', {
  testServer(summarise_server, {

    session$userData$dt_names <- reactive('iris')
    session$userData$dt$dt[['iris']] <- dt

    session$setInputs(
      dt_sel = 'iris',
      vars_sel = 'Petal.Length',
      fun_sel = 'distinct',
      txt_new_dt_name = 'new_dt'
    )

    session$setInputs(btn_summarise = 1)

    expect_equal(
      session$userData$dt$dt[['new_dt']],
      dt[, .SD, .SDcols = 'Petal.Length'] |> unique()
    )
  })
})
