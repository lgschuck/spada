# tests/testthat/test-save_gt_module.R

# test save gt - save ---------------------------------------------------------
test_that('Test save gt - html', {

  testServer(save_gt_server, args = list(input_table = reactive(iris |> gt())), {

    session$setInputs(
      file_name = 'gt_iris',
      radio_format = 'html'
    )

    expect_equal(output$down_handler |> basename(), 'gt_iris.html')

  })
})

test_that('Test save gt - rtf', {

  testServer(save_gt_server, args = list(input_table = reactive(iris |> gt())), {

    session$setInputs(
      file_name = 'gt_iris',
      radio_format = 'rtf'
    )

    expect_equal(output$down_handler |> basename(), 'gt_iris.rtf')

  })
})

# test save gt - check input_table --------------------------------------------
test_that('Test save gt - html', {

  testServer(save_gt_server, args = list(input_table = reactive(iris |> gt())), {

    session$setInputs(
      file_name = 'gt_iris',
      radio_format = 'html'
    )

    expect_true(input_table |> is.reactive())
    expect_equal(input_table() |> class(), c('gt_tbl', 'list'))

  })
})
