#' Print the cSEM GUI user guide
#'
#' Prints the step-by-step user guide to the R console, or opens it as a
#' vignette. The guide explains the model builder, every analysis in the
#' Calculate menu, how to read the report tables and how to export results.
#'
#' @param vignette If `TRUE`, open the HTML vignette instead (needs the
#'   vignettes to be installed). If the vignette is not available, the guide is
#'   printed to the console.
#'
#' @return Invisibly, the guide as a character vector (one element per line).
#' @seealso [run_csem_gui()]
#' @examples
#' csem_gui_guide()
#' @export
csem_gui_guide <- function(vignette = FALSE) {
  f <- system.file("guide", "user-guide.md", package = "csemapp")
  if (!nzchar(f)) stop("The user guide file was not found in the installed package.", call. = FALSE)
  txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
  if (isTRUE(vignette)) {
    v <- tryCatch(utils::vignette("csemapp-guide", package = "csemapp"), error = function(e) NULL)
    if (!is.null(v) && length(v$file)) { print(v); return(invisible(txt)) }
    message("Vignette not installed; printing the guide to the console instead.")
  }
  cat(txt, sep = "\n")
  invisible(txt)
}
