# tests/testthat/test-join_module.R

dt1 <- data.table(char_x = LETTERS[1:3], num = 1:3)
dt2 <- data.table(char_y = LETTERS[2:4], num = 10:12)

datasets <- list('dt1' = dt1, 'dt2' = dt2)

# test joins - new ------------------------------------------------------------
test_that('Test joins - new', {
  testServer(join_server, {

    session$userData$dt <- reactiveValues(
      dt = datasets,
      act_name = 'dt1'
    )

    session$userData$dt_names <- reactive({ names(session$userData$dt$dt) })

    session$setInputs(
      dt1_sel = 'dt1',
      vars_sel1 = 'char_x',
      dt2_sel = 'dt2',
      vars_sel2 = 'char_y',
      join_type = 'left',
      radio_overwrite = 'new',
      txt_new_dt_name = 'left_dt',
      btn_apply = 1
    )

    session$setInputs(
      join_type = 'right',
      txt_new_dt_name = 'right_dt',
      btn_apply = 2
    )

    session$setInputs(
      join_type = 'full',
      txt_new_dt_name = 'full_dt',
      btn_apply = 3
    )

    session$setInputs(
      join_type = 'inner',
      txt_new_dt_name = 'inner_dt',
      btn_apply = 4
    )

    # left join
    expect_s3_class(session$userData$dt$dt [[ 'left_dt' ]], 'data.table')
    expect_equal(session$userData$dt$dt [['left_dt']],
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'left'),
                 ignore_attr = TRUE)
    # right join
    expect_s3_class(session$userData$dt$dt [[ 'right_dt' ]], 'data.table')
    expect_equal(session$userData$dt$dt [['right_dt']],
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'right'),
                 ignore_attr = TRUE)

    # full join
    expect_s3_class(session$userData$dt$dt [[ 'full_dt' ]], 'data.table')
    expect_equal(session$userData$dt$dt [['full_dt']],
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'full'),
                 ignore_attr = TRUE)

    # inner join
    expect_s3_class(session$userData$dt$dt [[ 'inner_dt' ]], 'data.table')
    expect_equal(session$userData$dt$dt [['inner_dt']],
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'inner'),
                 ignore_attr = TRUE)

  })
})

# test joins - overwrite ------------------------------------------------------
test_that('Test joins - overwrite - left', {
  testServer(join_server, {

    session$userData$dt <- reactiveValues(
      dt = datasets,
      act_name = 'dt1'
    )

    session$userData$data_changed <- reactiveVal(0)
    session$userData$dt_names <- reactive({ names(session$userData$dt$dt) })

    session$setInputs(
      dt1_sel = 'dt1',
      vars_sel1 = 'char_x',
      dt2_sel = 'dt2',
      vars_sel2 = 'char_y',
      join_type = 'left',
      radio_overwrite = 'overwrite',
      btn_apply = 1
    )

    expect_s3_class(session$userData$dt$dt [[ 'dt1' ]], 'data.table')
    expect_equal(session$userData$dt$dt [['dt1']],
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'left'),
                 ignore_attr = TRUE)
  })
})

test_that('Test joins - overwrite - right', {
  testServer(join_server, {

    session$userData$dt <- reactiveValues(
      dt = datasets,
      act_name = 'dt1'
    )

    session$userData$data_changed <- reactiveVal(0)
    session$userData$dt_names <- reactive({ names(session$userData$dt$dt) })

    session$setInputs(
      dt1_sel = 'dt1',
      vars_sel1 = 'char_x',
      dt2_sel = 'dt2',
      vars_sel2 = 'char_y',
      join_type = 'right',
      radio_overwrite = 'overwrite',
      btn_apply = 1
    )

    expect_s3_class(session$userData$dt$dt [[ 'dt1' ]], 'data.table')
    expect_equal(session$userData$dt$dt [['dt1']],
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'right'),
                 ignore_attr = TRUE)
  })
})

test_that('Test joins - overwrite - full', {
  testServer(join_server, {

    session$userData$dt <- reactiveValues(
      dt = datasets,
      act_name = 'dt1'
    )

    session$userData$data_changed <- reactiveVal(0)
    session$userData$dt_names <- reactive({ names(session$userData$dt$dt) })

    session$setInputs(
      dt1_sel = 'dt1',
      vars_sel1 = 'char_x',
      dt2_sel = 'dt2',
      vars_sel2 = 'char_y',
      join_type = 'full',
      radio_overwrite = 'overwrite',
      btn_apply = 1
    )

    expect_s3_class(session$userData$dt$dt [[ 'dt1' ]], 'data.table')
    expect_equal(session$userData$dt$dt [['dt1']],
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'full'),
                 ignore_attr = TRUE)
  })
})


test_that('Test joins - overwrite - inner', {
  testServer(join_server, {

    session$userData$dt <- reactiveValues(
      dt = datasets,
      act_name = 'dt1'
    )

    session$userData$data_changed <- reactiveVal(0)
    session$userData$dt_names <- reactive({ names(session$userData$dt$dt) })

    session$setInputs(
      dt1_sel = 'dt1',
      vars_sel1 = 'char_x',
      dt2_sel = 'dt2',
      vars_sel2 = 'char_y',
      join_type = 'inner',
      radio_overwrite = 'overwrite',
      btn_apply = 1
    )

    expect_s3_class(session$userData$dt$dt [[ 'dt1' ]], 'data.table')
    expect_equal(session$userData$dt$dt [['dt1']],
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'inner'),
                 ignore_attr = TRUE)
  })
})

# test sample trigger reactiveVal ---------------------------------------------
test_that("Test Sample trigger - updates on button click", {
  testServer(join_server, {
    session$userData$data_changed <- reactiveVal(0)
    trigger_before <- preview_sample_trigger()

    session$setInputs(btn_preview_sample = 1)
    trigger_after <- preview_sample_trigger()

    expect_true(trigger_after > trigger_before)
  })
})

# test joins - preview df -----------------------------------------------------
test_that('Test joins - preview', {
  testServer(join_server, {

    session$userData$dt <- reactiveValues(
      dt = datasets,
      act_name = 'dt1'
    )

    preview_sample_trigger <- reactiveVal(0)

    session$setInputs(
      dt1_sel = 'dt1',
      vars_sel1 = 'char_x',
      dt2_sel = 'dt2',
      vars_sel2 = 'char_y',
      join_type = 'left',
      btn_preview_sample = 1
    )

    # left join
    expect_s3_class(preview_df(), 'data.table')
    expect_equal(preview_df(),
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'left'),
                 ignore_attr = TRUE)

    # right join
    session$setInputs(join_type = 'right', btn_preview_sample = 2)

    expect_s3_class(preview_df(), 'data.table')
    expect_equal(preview_df(),
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'right'),
                 ignore_attr = TRUE)

    # full join
    session$setInputs(join_type = 'full', btn_preview_sample = 3)

    expect_s3_class(preview_df(), 'data.table')
    expect_equal(preview_df(),
                 dt_join(dt1, dt2, 'char_x', 'char_y', 'full'),
                 ignore_attr = TRUE)
  })
})

# test errors -----------------------------------------------------------------
test_that('Preview_df returns validation error', {
  testServer(join_server, {

    session$userData$dt <- reactiveValues(
      dt = datasets,
      act_name = 'dt1'
    )
    session$setInputs(
      dt1_sel = 'dt1',
      dt2_sel = 'dt2',
      vars_sel1 = 'a',
      vars_sel2 = 'b',
      join_type = 'left'
    )

    expect_error(
      preview_df(),
      class = 'shiny.silent.error'
    )

  })
})

# test messages ---------------------------------------------------------------
test_that('Apply join - check inputs', {
  testServer(join_server, {

    session$userData$dt <- reactiveValues(
      dt = datasets,
      act_name = 'dt1'
    )

    session$userData$dt_names <- reactive({ names(session$userData$dt$dt) })

    last_msg <- NULL

    local_mocked_bindings(
      msg_error = function(text, ...) { last_msg <<- text },
      msg = function(text, ...) { last_msg <<- text }
    )

    # equal datasets
    session$setInputs(
      dt1_sel = 'dt1',
      dt2_sel = 'dt1',
      btn_apply = 1
    )

    expect_equal(last_msg, 'Select two diferent datasets')

    # selected variables
    session$setInputs(
      dt1_sel = 'dt1',
      dt2_sel = 'dt2',
      btn_apply = 2
    )

    expect_equal(last_msg, 'Select variables for both datasets')

    # lenght of selected variables
    session$setInputs(
      vars_sel1 = c('char_x', 'num'),
      vars_sel2 = c('num'),
      btn_apply = 3
    )

    expect_equal(last_msg, 'The number of selected variables must has the same for both datasets')

    # new name not valid
    session$setInputs(
      vars_sel1 = 'char_x',
      vars_sel2 = 'char_y',
      radio_overwrite = 'new',
      txt_new_dt_name = 123,
      btn_apply = 4
    )

    expect_equal(last_msg, 'New name is not valid or already in use')

    # new name already in use
    session$setInputs(
      txt_new_dt_name = 'dt1',
      btn_apply = 5
    )

    expect_equal(last_msg, 'New name is not valid or already in use')

    # invalid vars
    session$setInputs(
      vars_sel2 = 'char_y2',
      txt_new_dt_name = 'new_dt',
      btn_apply = 6
    )

    expect_equal(last_msg, 'Select valid variables')

    # type of vars must match
    session$setInputs(
      vars_sel1 = 'char_x',
      vars_sel2 = 'num',
      btn_apply = 7
    )

    expect_equal(last_msg, 'The variables types must match')

    # join applied
    session$setInputs(
      vars_sel1 = 'char_x',
      vars_sel2 = 'char_y',
      btn_apply = 8
    )

    expect_equal(last_msg, 'Join applied')

  })
})
