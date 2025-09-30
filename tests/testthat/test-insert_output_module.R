# tests/testthat/test-insert_output_module.R

# test insert output ----------------------------------------------------------
test_that("Test insert outuput", {
  testServer(insert_output_server, args = list(reactive('Content')), {

    # Add to output
    session$setInputs(btn_add_output = 1)
    expect_true(is.reactive(output_card))

    # Values inserted in modal
    session$setInputs(output_title = 'Title')
    session$setInputs(output_annot = 'Annotation')

    session$setInputs(btn_confirm_add_output = 1)

    expect_type(output_card(), 'list')
    expect_equal(output_card() |> class(), 'shiny.tag')
    expect_equal(output_card() |> length(), 3)
    expect_equal(output_card(), report_card('Title', 'Annotation', 'Content', output_id()))
  })
})
