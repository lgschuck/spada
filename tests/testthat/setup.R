# tests/testthat/setup.R

mirai::daemons(1)

spada_everywhere()

# withr::defer(
#   mirai::daemons(0),
#   envir = testthat::teardown_env()
# )
