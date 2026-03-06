# tests/testthat/test-table_values_module.R

# test table values module - tables -------------------------------------------
test_that('Tables values - 1 variable', {
  testServer(table_values_server,
             args = list(reactive(iris$Species),
                         reactive(iris$Petal.Width),
                         reactive('Species'),
                         reactive('Petal.Width')
                         ), {

    session$setInputs(
      table_var = '1v',
      table_type = 'abs_table'
    )

    expect_equal(var(), iris$Species)
    expect_equal(table_values(), table(iris$Species) |> as.data.frame())
    expect_s3_class(table_values(), 'data.frame')
    expect_s3_class(table_values_gt(), 'gt_tbl')
  })
})

test_that('Exploratory module - tables - 2 variables', {
  testServer(table_values_server,
             args = list(reactive(mtcars$cyl |> as.integer() |> as.factor()),
                         reactive(mtcars$am |> as.integer() |> as.factor()),
                         reactive('cyl'),
                         reactive('am')
             ), {

    session$setInputs(
      table_var = '2v',
      table_type = 'abs_table'
    )

    tab1 <- table(mtcars$cyl |> as.integer() |> as.factor(),
                  mtcars$am |> as.integer() |> as.factor())
    tab1 <- tab1 |> as.data.frame.matrix()
    tab1 <- cbind(var1 = rownames(tab1), tab1)

    expect_equal(var(), mtcars$cyl |> as.integer() |> as.factor())
    expect_equal(table_values(), tab1)
    expect_s3_class(table_values(), 'data.frame')
    expect_s3_class(table_values_gt(), 'gt_tbl')
  })
})
