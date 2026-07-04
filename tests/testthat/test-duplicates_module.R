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
      vars_sel_dup = NULL,
      txt_new_dup_name = 'Duplicated',
      radio_last = 'FALSE',
      radio_primary = 'FALSE',
      txt_new_primary_name = 'Primary',
      btn_identify = 1
    )

    expect_equal(last_msg, 'Select at least one variable')

    session$setInputs(
      vars_sel_dup = 'Species',
      txt_new_dup_name = 'Species',
      btn_identify = 2
    )

    expect_equal(last_msg, 'New names are not valid or already in use')

    session$setInputs(
      txt_new_dup_name = 'Species2',
      txt_new_primary_name = 'Species',
      btn_identify = 3
    )

    expect_equal(last_msg, 'New names are not valid or already in use')

    # remove duplicates
    session$setInputs(
      vars_sel_drop = 'Species',
      btn_remove = 2
    )

    expect_equal(last_msg, 'The variable is not a factor of Duplicated and Not Duplicated Levels')

  })
})

# test duplicates - identify --------------------------------------------------
test_that('Test duplicates', {
  testServer(duplicates_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel_dup = 'Species',
      txt_new_dup_name = 'Duplicated',
      radio_last = 'FALSE',
      radio_primary = 'TRUE',
      txt_new_primary_name = 'Primary',
      btn_identify = 1
    )

    expect_equal(
      get_act_dt(session)$Duplicated,
      factor(
        duplicated(iris_dt, by = 'Species'),
        levels = c(T, F),
        labels = c('Duplicated', 'Not Duplicated')
      )
    )

    expect_equal(
      get_act_dt(session)$Primary,
      factor(
        !duplicated(iris_dt, by = 'Species'),
        levels = c(T, F),
        labels = c('Primary', 'Not Primary')
      )
    )

    session$setInputs(
      radio_last = 'TRUE',
      txt_new_dup_name = 'Duplicated2',
      txt_new_primary_name = 'Primary2',
      btn_identify = 2
    )

    expect_equal(
      get_act_dt(session)$Duplicated2,
      factor(
        duplicated(iris_dt, fromLast = TRUE, by = 'Species'),
        levels = c(T, F),
        labels = c('Duplicated', 'Not Duplicated')
      )
    )

    expect_equal(
      get_act_dt(session)$Primary2,
      factor(
        !duplicated(iris_dt, fromLast = TRUE, by = 'Species'),
        levels = c(T, F),
        labels = c('Primary', 'Not Primary')
      )
    )
  })
})

# test duplicates - remove ----------------------------------------------------
test_that('Test duplicates', {
  testServer(duplicates_server, {

    session$userData$dt <- reactiveValues(
      dt = list('iris' = iris_dt),
      act_name = 'iris'
    )

    session$userData$data_changed <- reactiveVal(0)

    session$setInputs(
      vars_sel_dup = 'Species',
      txt_new_dup_name = 'Duplicated',
      radio_last = 'FALSE',
      radio_primary = 'FALSE',
      btn_identify = 1
    )

    session$setInputs(
      vars_sel_drop = 'Duplicated',
      btn_remove = 2
    )

    expect_equal(get_act_dt(session) |> nrow(), 3)
  })
})


