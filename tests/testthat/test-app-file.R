app_file <- function() system.file("app", "app.R", package = "csemapp")

test_that("the app script parses without syntax errors", {
  expect_error(parse(file = app_file(), encoding = "UTF-8"), NA)
})

test_that("the app defines the expected tabs and analyses", {
  src <- paste(readLines(app_file(), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (tab in c("Model builder", "Results diagram", "cSEM report", "Bootstrapping report",
                "Confirmatory tetrad analysis", "IPMA", "PLSpredict", "CVPAT")) {
    expect_true(grepl(paste0("tabPanel(\"", tab, "\""), src, fixed = TRUE), info = tab)
  }
  expect_true(grepl("shinyApp(ui, server)", src, fixed = TRUE))
})

test_that("the user guide mentions every analysis of the Calculate menu", {
  guide <- paste(readLines(system.file("guide", "user-guide.md", package = "csemapp"),
                           warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (topic in c("PLS-SEM algorithm", "Consistent PLS", "Bootstrapping", "IPMA",
                  "Confirmatory tetrad analysis", "PLSpredict", "CVPAT")) {
    expect_true(grepl(topic, guide, fixed = TRUE), info = topic)
  }
})
