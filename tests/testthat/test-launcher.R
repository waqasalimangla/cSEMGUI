test_that("exported functions exist", {
  expect_true(is.function(run_csem_gui))
  expect_true(is.function(csem_gui_app))
  expect_true(is.function(csem_gui_guide))
})

test_that("the app script is installed with the package", {
  f <- system.file("app", "app.R", package = "csemapp")
  expect_true(nzchar(f))
  expect_true(file.exists(f))
})

test_that("the user guide can be printed", {
  expect_output(out <- csem_gui_guide(), "cSEM")
  expect_type(out, "character")
  expect_gt(length(out), 20)
})

test_that("csem_gui_app() returns a shiny app object", {
  skip_on_cran()
  skip_if_not_installed("cSEM")
  skip_if_not_installed("writexl")
  skip_if_not_installed("jsonlite")
  expect_s3_class(csem_gui_app(), "shiny.appobj")
})
