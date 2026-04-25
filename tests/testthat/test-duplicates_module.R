# tests/testthat/test-duplicates_module.R

iris_dt <- iris |> as.data.table()

# test duplicates - check messages --------------------------------------------
test_that('Test duplicates - check inputs', {
  testServer(duplicates_server, {

    last_msg <- NULL

    local_mocked_bindings(
      msg = function(text, ...) { last_msg <<- text },
      msg_error = function(text, ...) { last_msg <<- text }
    )

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel = NULL,
      txt_new_dup_name = 'Duplicated',
      radio_last = 'FALSE',
      radio_primary = 'FALSE',
      txt_new_primary_name = 'Primary',
      btn_apply = 1
    )

    expect_equal(last_msg, 'Select at least one variable')

    session$setInputs(
      vars_sel = 'Species',
      txt_new_dup_name = 'Species',
      btn_apply = 2
    )

    expect_equal(last_msg, 'New names are not valid or already in use')

    session$setInputs(
      txt_new_dup_name = 'Species2',
      txt_new_primary_name = 'Species',
      btn_apply = 3
    )

    expect_equal(last_msg, 'New names are not valid or already in use')
  })
})


# test duplicates -------------------------------------------------------------
test_that('Test duplicates', {
  testServer(duplicates_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel = 'Species',
      txt_new_dup_name = 'Duplicated',
      radio_last = 'FALSE',
      radio_primary = 'TRUE',
      txt_new_primary_name = 'Primary',
      btn_apply = 1
    )

    expect_equal(get_act_dt(session)$Duplicated,
                 duplicated(iris_dt, by = 'Species') |> as.integer())

    expect_equal(get_act_dt(session)$Primary,
                 as.integer(!duplicated(iris_dt, by = 'Species')))

    session$setInputs(
      radio_last = 'TRUE',
      txt_new_dup_name = 'Duplicated2',
      txt_new_primary_name = 'Primary2',
      btn_apply = 2
    )

    expect_equal(get_act_dt(session)$Duplicated2,
                 duplicated(iris_dt, fromLast = TRUE, by = 'Species') |> as.integer())

    expect_equal(get_act_dt(session)$Primary2,
                 as.integer(!duplicated(iris_dt, fromLast = TRUE,by = 'Species')))

  })
})


