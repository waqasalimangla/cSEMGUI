# Directory of the installed application (contains app.R, the original cSEM GUI script)
app_dir <- function() {
  d <- system.file("app", package = "csemapp")
  if (!nzchar(d) || !file.exists(file.path(d, "app.R")))
    stop("The application files were not found in the installed 'csemapp' package. Please reinstall it.",
         call. = FALSE)
  d
}

# The app needs these packages; tell the user exactly what is missing instead of failing half-way
check_app_packages <- function() {
  need <- c("shiny", "cSEM", "writexl", "jsonlite", "parallel")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss))
    stop("Missing package(s): ", paste(miss, collapse = ", "), ".\nInstall with: install.packages(c(",
         paste0("\"", miss, "\"", collapse = ", "), "))", call. = FALSE)
  invisible(TRUE)
}

# (references so that R CMD check sees every declared Import as used; never called)
.declared_imports <- function() list(cSEM::csem, writexl::write_xlsx, jsonlite::fromJSON, utils::packageVersion)

#' Launch the cSEM GUI
#'
#' Starts the interactive Shiny application: a point-and-click interface for
#' PLS-SEM with the \pkg{cSEM} package (model builder, PLS / consistent PLS,
#' parallel bootstrapping, IPMA, confirmatory tetrad analysis, PLSpredict and
#' CVPAT, reports with Excel export). The application script is shipped
#' unchanged in the package (`inst/app/app.R`).
#'
#' `run_csem_gui()` starts the app on this computer and blocks the R console
#' until the app is stopped (press Esc / the stop button). `csem_gui_app()`
#' returns the app as a [shiny::shinyApp()] object instead, which is what a
#' server needs: put `csemapp::csem_gui_app()` into an `app.R` file for
#' shiny-server, shinyapps.io or Posit Connect.
#'
#' Call [csem_gui_guide()] for a step-by-step user guide.
#'
#' @param launch.browser Open the app in the default web browser? Default `TRUE`.
#' @param port Port to listen on. `NULL` (default) lets Shiny pick a free port.
#' @param host Host/IP address. Default `"127.0.0.1"` (this computer only).
#' @param ... Further arguments passed to [shiny::runApp()].
#'
#' @return `run_csem_gui()` runs until the app is stopped and returns `NULL`
#'   invisibly. `csem_gui_app()` returns a [shiny::shinyApp()] object.
#' @seealso [csem_gui_guide()]
#' @examples
#' if (interactive()) run_csem_gui()
#' @export
run_csem_gui <- function(launch.browser = TRUE, port = getOption("shiny.port"),
                         host = getOption("shiny.host", "127.0.0.1"), ...) {
  check_app_packages()
  shiny::runApp(app_dir(), launch.browser = launch.browser, port = port, host = host, ...)
  invisible(NULL)
}

#' @rdname run_csem_gui
#' @export
csem_gui_app <- function() {
  check_app_packages()
  shiny::shinyAppDir(app_dir())
}
