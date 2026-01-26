# tests/testthat/test-summarise_module.R

dt <- iris |> as.data.table()

# test summarise - distinct ---------------------------------------------------
test_that('Test summarise - new dt - distinct function', {
  testServer(summarise_server, {

    session$userData$dt_names <- reactive('iris')
    session$userData$dt$dt[['iris']] <- dt
    session$userData$dt$act_name <- 'iris'

    session$setInputs(
      vars_sel = 'Petal.Length',
      fun_sel = 'distinct',
      radio_overwrite = 'new',
      txt_new_dt_name = 'new_dt'
    )

    session$setInputs(btn_summarise = 1)

    expect_equal(
      session$userData$dt$dt[['new_dt']],
      dt[, .SD, .SDcols = 'Petal.Length'] |> unique()
    )
  })
})
