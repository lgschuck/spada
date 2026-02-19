# tests/testthat/test-insert_output_module.R

# test insert output ----------------------------------------------------------
test_that("Test insert outuput", {
  testServer(insert_output_server, args = list(reactive('Content')), {

    # Add to output
    session$setInputs(btn_add_output = 1)
    # expect_true(is.reactive(output_card))

    # Values inserted in modal
    session$setInputs(output_title = 'Title')
    session$setInputs(output_annot = 'Annotation')

    session$setInputs(btn_confirm_add_output = 1)

    # check names
    expect_true(grepl('id_', session$userData$out$elements |> names()))
    # check id
    expect_type(session$userData$out$elements[[1]][[1]], 'character')
    expect_true(grepl('id_', session$userData$out$elements[[1]][[1]]))
    # check title
    expect_type(session$userData$out$elements[[1]][[2]], 'character')
    expect_true(session$userData$out$elements[[1]][[2]] == 'Title')
    # check card
    expect_type(session$userData$out$elements[[1]][[3]], 'list')
    expect_true(grepl('Content', session$userData$out$elements[[1]][[3]]))
    # check btn
    expect_type(session$userData$out$elements[[1]][[4]], 'list')
    expect_true(
      grepl("btn btn-default action-button micro-btn-cancel",
            session$userData$out$elements[[1]][[4]]
            )
    )

  })
})
